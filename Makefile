ai:
	ocamlbuild -pkgs oUnit ai_controller.byte

aitest:
	ocamlbuild -pkgs oUnit,str,unix aitest.byte && ./aitest.byte

display:
	ocamlbuild -pkgs oUnit display_controller.byte	
