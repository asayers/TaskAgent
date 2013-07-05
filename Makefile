todo: todo.hs
	ghc -O2 -dynamic todo.hs
	strip -s todo
	cp todo ~/bin/todo
