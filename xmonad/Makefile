all: setup_links sys_info xmonad_recompile
	echo "[make build] for clean build"

build: setup_links sys_info xmonad_recompile working_clean


sys_info: src/sys_info.c
	gcc src/sys_info.c -o sys_info

setup_links:
	ln -f -sT src/xmonad.hs xmonad.hs

xmonad_recompile:
	xmonad --recompile

working_clean:
	rm -f *.pyc
	rm -f xmonad.hi
	rm -f xmonad.o
	rm -f xmonad.errors

remove_all: working_clean
	rm -f xmonad-*-linux
	rm -f history
	rm -f sys_info

clean: remove_all
