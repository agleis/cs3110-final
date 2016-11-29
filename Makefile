ai:
	ocamlbuild -pkgs oUnit ai_controller.byte

aitest:
	ocamlbuild -pkgs oUnit,str,unix aitest.byte && ./aitest.byte

game:
		ocamlbuild game_controller.byte && ./game_controller.byte

clean:
		ocamlbuild -clean

display:
	ocamlbuild -tag custom -lib graphics -pkgs oUnit display_controller.byte && ./display_controller.byte
