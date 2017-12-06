Tree Parser::parse_type_declaration (){
        // type_declaration -> "type" identifier ":" type ";"
        if (!skip_token (Ptiger::TYPE)) {
                skip_after_end ();
                return Tree::error ();
        }

        const_TokenPtr identifier = expect_token (Ptiger::IDENTIFIER);
        if (identifier == NULL) {
                skip_after_end ();
                return Tree::error ();
        }

        std::string name;
        Tree expr;

        const_TokenPtr t = lexer.peek_token();

        if(t->get_id() == Ptiger::COLON) {
                // if (!skip_token (Ptiger::COLON)) {
                //         skip_after_end ();
                //         return Tree::error ();
                // }
                lexer.skip_token();

                Tree type_tree = parse_type();

                if (type_tree.is_error ()) {
                        skip_after_end();
                        return Tree::error();
                }

                //skip_token (Ptiger::SEMICOLON);

                if (scope.get_current_mapping ().get (identifier->get_str ())) {
                        error_at (identifier->get_locus (),
                                  "name '%s' already declared in this scope",
                                  identifier->get_str ().c_str ());
                }

                SymbolPtr sym (new Symbol (Ptiger::TYPENAME, identifier->get_str ()));
                scope.get_current_mapping ().insert (sym);

                name = sym->get_name ().c_str ();


        } else {
                if (!skip_token (Ptiger::EQ)) {
                        skip_after_end ();
                        return Tree::error ();
                }

                const_TokenPtr t = lexer.peek_token();
                if(t->get_id() == Ptiger::LBRACK) {

                        Tree type_tree = parse_type_record ();
                        if (type_tree.is_error ()) {
                                skip_after_end();
                                return Tree::error ();
                        }
                        //skip_token (Ptiger::SEMICOLON);
                        if (scope.get_current_mapping ().get (identifier->get_str ())) {
                                error_at (identifier->get_locus (),
                                          "name '%s' already declared in this scope",
                                          identifier->get_str ().c_str ());
                        }
                        SymbolPtr sym (new Symbol (Ptiger::TYPENAME, identifier->get_str ()));
                        scope.get_current_mapping ().insert (sym);
                        name = sym->get_name ().c_str ();

                }else if(t->get_id() == Ptiger::ARRAY) {
                        skip_token(Ptiger::ARRAY);
                        skip_token(Ptiger::OF);

                        if (scope.get_current_mapping ().get (identifier->get_str ())) {
                                error_at (identifier->get_locus (),
                                          "type '%s' already declared in this scope",
                                          identifier->get_str ().c_str ());
                        }
                        t = lexer.peek_token();
                        if(t->get_id() == Ptiger::INT) {
                                SymbolPtr sym (new Symbol (Ptiger::TYPEARRAYI, identifier->get_str ()));
                                Tree type_tree = parse_type ();
                                if (type_tree.is_error ()) {
                                        skip_after_end();
                                        return Tree::error ();
                                }
                                scope.get_current_mapping ().insert (sym);

                                name = sym->get_name ().c_str ();
                        }
                        else if(t->get_id() == Ptiger::REAL) {
                                SymbolPtr sym (new Symbol (Ptiger::TYPEARRAYR, identifier->get_str ()));
                                Tree type_tree = parse_type ();
                                if (type_tree.is_error ()) {
                                        skip_after_end();
                                        return Tree::error ();
                                }
                                scope.get_current_mapping ().insert (sym);

                                name = sym->get_name ().c_str ();
                        }
                        else if(t->get_id() == Ptiger::STRING) {
                                SymbolPtr sym (new Symbol (Ptiger::TYPEARRAYS, identifier->get_str ()));
                                Tree type_tree = parse_type ();
                                if (type_tree.is_error ()) {
                                        skip_after_end();
                                        return Tree::error ();
                                }
                                scope.get_current_mapping ().insert (sym);

                                name = sym->get_name ().c_str ();
                        }
                }
                return Tree::error();
        }

        Tree decl = build_decl (identifier->get_locus (), TYPE_DECL,
                                get_identifier (name),
                                type_tree.get_tree ());
        DECL_CONTEXT (decl.get_tree()) = main_fndecl;
        gcc_assert (!stack_var_decl_chain.empty ());
        stack_var_decl_chain.back ().append (decl);
        sym->set_tree_decl (decl);

        expr = build_tree (DECL_EXPR, identifier->get_locus (), void_type_node, decl);
        return expr;
}
