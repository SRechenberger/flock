docs: lib/Flock.hs lib/Simulation.hs stack.yaml
	stack haddock --no-haddock-deps
	cp -r .stack-work/install/x86_64-linux/`cat stack.yaml | sed -n '/#/! s/resolver: \(\.*\)/\1/p'`/8.0.2/doc/flocking-0.1.0.0/* docs

