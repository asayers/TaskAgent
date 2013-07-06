todo: todo.hs
	ghc -O2 todo.hs
	strip -s todo

install: todo
	cp todo ~/bin/todo
