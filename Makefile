.PHONY: help install egg-herbie nightly index start-server deploy

help:
	@echo "Type 'make install' to install Herbie"
	@echo "Then type 'racket -l herbie web' to run it."

install: clean egg-herbie update

clean:
	raco pkg remove --force --no-docs herbie && echo "Uninstalled old herbie" || :
	raco pkg remove --force --no-docs egg-herbie && echo "Uninstalled old egg-herbie" || :
	raco pkg remove --force --no-docs egg-herbie-linux && echo "Uninstalled old egg-herbie" || :
	raco pkg remove --force --no-docs egg-herbie-windows && echo "Uninstalled old egg-herbie" || :
	raco pkg remove --force --no-docs egg-herbie-osx && echo "Uninstalled old egg-herbie" || :
	raco pkg remove --force --no-docs egg-herbie-macosm1 && echo "Uninstalled old egg-herbie" || :

update:
	raco pkg install --skip-installed --no-docs --auto --name herbie src/
	raco pkg update --auto rival
	raco pkg update --name herbie --deps search-auto src/

egg-herbie:
	cargo build --release --manifest-path=egg-herbie/Cargo.toml
	raco pkg remove --force --no-docs egg-herbie && echo "Warning: uninstalling egg-herbie and reinstalling local version" || :
	raco pkg remove --force --no-docs egg-herbie-linux && echo "Warning: uninstalling egg-herbie and reinstalling local version" || :
	raco pkg remove --force --no-docs egg-herbie-windows && echo "Warning: uninstalling egg-herbie and reinstalling local version" || :
	raco pkg remove --force --no-docs egg-herbie-osx && echo "Warning: uninstalling egg-herbie and reinstalling local version" || :
	raco pkg remove --force --no-docs egg-herbie-macosm1 && echo "Warning: uninstalling egg-herbie and reinstalling local version" || :
	raco pkg install ./egg-herbie

distribution: minimal-distribution
	cp -r bench herbie-compiled/

minimal-distribution:
	mkdir -p herbie-compiled/
	cp README.md herbie-compiled/
	cp LICENSE.md herbie-compiled/
	cp logo.png herbie-compiled/
	raco exe -o herbie --orig-exe --embed-dlls --vv src/main.rkt
	[ ! -f herbie.exe ] || (raco distribute herbie-compiled herbie.exe && rm herbie.exe)
	[ ! -f herbie.app ] || (raco distribute herbie-compiled herbie.app && rm herbie.app)
	[ ! -f herbie ] || (raco distribute herbie-compiled herbie && rm herbie)

nightly: install
	bash infra/nightly.sh bench/ reports/ --threads 4
	bash infra/publish.sh upload reports/

upgrade:
	git pull
	$(MAKE) install

start-server:
	racket -y src/main.rkt web --seed 1 --timeout 150 --num-iters 2 \
		--demo --public --prefix /demo/ --port 4053 --save-session www/demo/ \
		--log infra/server.log --quiet 2>&1

fmt:
	@raco fmt -i $(shell find egg-herbie/ src/ infra/ -name '*.rkt')

herbie.zip herbie.zip.CHECKSUM:
	raco pkg create src/
	mv src.zip herbie.zip
	mv src.zip.CHECKSUM herbie.zip.CHECKSUM

random-file:
	@find src infra -path '*/compiled/*' -prune \
		    -o -path 'infra/survey/*' -prune \
		    -o -type f -print | \
	sort -R | head -n1
