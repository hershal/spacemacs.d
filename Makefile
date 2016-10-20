all:
	emacs --batch --eval '(progn (find-file "init-new.org") (org-babel-tangle))'
