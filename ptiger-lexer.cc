#include "ptiger-lexer.h"

#include "config.h"
#include "coretypes.h"
#include "input.h"
#include "diagnostic.h"
#include "safe-ctype.h"

#include <cstdlib>
#include <algorithm>

namespace Ptiger {

Lexer::Lexer (const char *filename, FILE *input_)
: input (input_), current_line (1), current_column (1), line_map (0),
input_source (input), input_queue (input_source), token_source (this),
token_queue (token_source) {
  line_map = ::linemap_add (::line_table, ::LC_ENTER,
    /* sysp */ 0, filename,
    /* current_line */ 1);
  }

Lexer::~Lexer () {
  ::linemap_add (::line_table, ::LC_LEAVE,
    /* sysp */ 0,
    /* filename */ NULL,
    /* to_line */ 0);
  }

location_t Lexer::get_current_location () {
  return ::linemap_position_for_column (::line_table, current_column);
}

int Lexer::peek_input (int n) {
  return input_queue.peek (n);
}

int Lexer::peek_input () {
  return peek_input (0);
}

void Lexer::skip_input (int n) {
  return input_queue.skip (n);
}

void Lexer::skip_input () {
  return skip_input (0);
}

namespace {
  const std::string keyword_index[] = {
    #define PTIGER_TOKEN(x, y)
    #define PTIGER_TOKEN_KEYWORD(name, keyword) keyword,
    PTIGER_TOKEN_LIST
    #undef PTIGER_TOKEN_KEYWORD
    #undef PTIGER_TOKEN
  };

  TokenId keyword_keys[] = {
    #define PTIGER_TOKEN(x, y)
    #define PTIGER_TOKEN_KEYWORD(name, keyword) name,
    PTIGER_TOKEN_LIST
    #undef PTIGER_TOKEN_KEYWORD
    #undef PTIGER_TOKEN
  };

  const int num_keywords = sizeof (keyword_index) / sizeof (*keyword_index);
}

TokenId Lexer::classify_keyword (const std::string &str) {
  const std::string *last = keyword_index + num_keywords;
  const std::string *idx = std::lower_bound (keyword_index, last, str);

  if (idx == last || str != *idx)
    return IDENTIFIER;
  else {
    return keyword_keys[idx - keyword_index];
  }
}

TokenPtr Lexer::build_token (){

  int open_comment;

  for (;;) {
    location_t loc = get_current_location ();
    int current_char = peek_input ();
    skip_input ();

    if (current_char == EOF) { //STOP GETING CHAR BY CHAR WHEN EOF
      return Token::make (END_OF_FILE, loc);
    }

    switch (current_char) {
      //WHITESPACE
      case '\n':
        current_line++;
        current_column = 1;
        linemap_line_start (::line_table, current_line, max_column_hint);
        continue;
      case ' ':
        // printf("whitespace!\n");
        current_column++;
        continue;
      case '\t':
        // depend on tab configuration
        current_column += 8;
        continue;
      //PUNCT
      case ':':
        if (peek_input () == '=') {
          skip_input ();
          current_column += 2;
          return Token::make (ASSIGN, loc);
        }
        else {
          current_column++;
          return Token::make (COLON, loc);
        }
      case '*':
        if (peek_input () == '/') {
          skip_input ();
          current_column += 2;
          error_at (get_current_location (), "extraneous comment mark");
        }else{
          current_column++;
          return Token::make (TIMES, loc);
        }
      case ',':
        current_column++;
        return Token::make (COMMA, loc);
      case '=':
        current_column++;
        return Token::make (EQ, loc);
      case '(':
        current_column++;
        return Token::make (LPAREN, loc);
      case '-':
        current_column++;
        return Token::make (MINUS, loc);
      case '+':
        current_column++;
        return Token::make (PLUS, loc);
      case ')':
        current_column++;
        return Token::make (RPAREN, loc);
      case ';':
        current_column++;
        return Token::make (SEMICOLON, loc);
      case '&':
        current_column++;
        return Token::make (AND, loc);
      case '|':
        current_column++;
        return Token::make (OR, loc);
      case '<':
        if (peek_input () == '=') {
          skip_input ();
          current_column += 2;
          return Token::make (LE, loc);
        }
        else if (peek_input () == '>') {
          skip_input ();
          current_column += 2;
          return Token::make (NEQ, loc);
        }
        else {
          current_column++;
          return Token::make (LT, loc);
        }
        break;
      case '>':
        if (peek_input () == '=') {
          skip_input ();
          current_column += 2;

          return Token::make (GE, loc);
        }
        else {
          current_column++;
          return Token::make (GT, loc);
        }
        break;
      case '/': /* comment */
        open_comment = 0;
        if (peek_input () == '*') {
          open_comment++;
          skip_input ();
          current_column += 2;
          while (open_comment != 0) {
            current_char = peek_input ();
            if(current_char == '/'){
              skip_input ();
              current_column++;
              current_char = peek_input ();
              skip_input ();
              current_column++;
              if (current_char == '*') {
                open_comment++;
              }
            }
            else if (current_char == '*'){
              skip_input ();
              current_column++;
              current_char = peek_input ();
              skip_input ();
              current_column++;
              if (current_char == '/') {
                open_comment--;
              }
            }
            else {
              skip_input ();
              current_column++;
            }

            if (current_char == EOF) { //STOP GETING CHAR BY CHAR WHEN EOF
              return Token::make (END_OF_FILE, loc);
            }

          }
          // if (peek_input () != '/') {
          //   // current_column++;
          //   error_at (get_current_location (), "unended comment mark");
          // }
          // else{
          //   skip_input ();
          // }
        }
        else {
          return Token::make (DIVIDE, loc);
        }
        continue;
        break;
      case '[':
        current_column++;
        return Token::make (LBRACE, loc);
      case ']':
        current_column++;
        return Token::make (RBRACE, loc);
      case '{':
        current_column++;
        return Token::make (LBRACK, loc);
      case '}':
        current_column++;
        return Token::make (RBRACK, loc);
      case '.':
        if (!ISDIGIT(peek_input ())) {
          // Only if followed by a non number
          current_column++;
          return Token::make (DOT, loc);
        }
    } //end switch

    //ID AND KEYWORDS
    if(ISALPHA (current_char) || current_char == '_') {
      std::string str;
      str.reserve (16); // some sensible default
      str += current_char;

      int length = 1;
      current_char = peek_input ();
      while (ISALPHA (current_char) || ISDIGIT (current_char) || current_char == '_') {
        length++;
        str += current_char;
        skip_input ();
        current_char = peek_input ();
      }

      current_column += length;

      TokenId keyword = classify_keyword (str);
      if (keyword == IDENTIFIER) {
        return Token::make_identifier (loc, str);
      }
      else {
        return Token::make (keyword, loc);
      }
    }

    //INTEGER LITERALS
    if (ISDIGIT (current_char) || current_char == '.') {
    	std::string str;
    	str.reserve (16); // some sensible default
    	str += current_char;

    	bool is_real = (current_char == '.');

    	int length = 1;
    	current_char = peek_input ();
    	while (ISDIGIT (current_char) || (!is_real && current_char == '.')) {
    		length++;

    		is_real = is_real || (current_char == '.');

    		str += current_char;
    		skip_input ();
    		current_char = peek_input ();
    	}

    	current_column += length;

    	if (is_real){
    		return Token::make_real (loc, str);
    	}
    	else {
    		return Token::make_integer (loc, str);
    	}
    }

    //STRING LITERALS
    if (current_char == '"') {
      std::string str;
      str.reserve (16); // some sensible default

      int length = 1;
      current_char = peek_input ();
      while (current_char != '\n' && current_char != '"') {
        length++;

        str += current_char;
        skip_input ();
        current_char = peek_input ();
      }

      current_column += length;

      if (current_char == '\n') {
        error_at (get_current_location (), "unended string literal");
      }
      else if (current_char == '"') {
        skip_input ();
      }
      else {
        gcc_unreachable ();
      }

      return Token::make_string (loc, str);
    }
    // ALIEEEEENS
    error_at (loc, "unexpected character '%x'", current_char);
    current_column++;
  }//end for
}//end Lexer::build_token

const_TokenPtr Lexer::peek_token (int n) {
  return token_queue.peek (n);
}

const_TokenPtr Lexer::peek_token () {
  return peek_token (0);
}

void Lexer::skip_token (int n) {
  token_queue.skip (n);
}

void Lexer::skip_token () {
  skip_token (0);
}

}
