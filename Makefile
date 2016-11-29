ai:
	ocamlbuild -pkgs oUnit ai_controller.byte

aitest:
	ocamlbuild -pkgs oUnit,str,unix aitest.byte && ./aitest.byte

display:
	ocamlbuild -tag custom -lib graphics -pkgs oUnit display_controller.byte && ./display_controller.byte
