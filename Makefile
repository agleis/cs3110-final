ai:
	ocamlbuild -pkgs oUnit ai_controller.byte

aitest:
	ocamlbuild -pkgs oUnit,str,unix aitest.byte && ./aitest.byte

game:
	ocamlbuild -pkgs graphics,unix game_controller.byte && ./game_controller.byte

clean:
	ocamlbuild -clean

display:
	ocamlbuild -pkgs oUnit,graphics display_controller.byte && ./display_controller.byte

setup:
	ocamlbuild -pkgs graphics,unix,str setup_controller.byte && clear && ./setup_controller.byte
