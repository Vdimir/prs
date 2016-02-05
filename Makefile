.PHONY: watch test commit gadd

watch:
	./w.sh test

test:
	cargo test

commit: test
	git commit

gadd: test
	git add src/*.rs tests/*.rs