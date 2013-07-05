todo: todo.hs
	ghc -O2 -dynamic todo.hs
	strip -s todo

install: todo
	cp todo ~/bin/todo
