.PHONY : run build
## You need to install the pipes library which could be installed through the repo.
## https://github.com/QuentinDuval/IdrisPipes

build :
	idris yieldStdLn.idr -p pipes -o yieldStdLn.x

buildGen:
	idris GenEffect.idr -p effects -o nextCont.x

run : build
	./yieldStdLn.x
