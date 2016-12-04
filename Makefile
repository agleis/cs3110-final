ai:
	ocamlbuild -pkgs oUnit ai_controller.byte

aitest:
	ocamlbuild -pkgs oUnit,str,unix aitest.byte && ./aitest.byte

game:
	ocamlbuild -pkgs graphics game_controller.byte && ./game_controller.byte

clean:
	ocamlbuild -clean

display:
	ocamlbuild -pkgs oUnit,graphics display_controller.byte && ./display_controller.byte

setup:
	ocamlbuild -pkgs graphics setup_controller.byte && ./setup_controller.byte
