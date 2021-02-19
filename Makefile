mini: mini.go
ifdef VERSION
	go build -ldflags="-X 'main.version=$(VERSION)'" -o mini .
else
	go build -o mini .
endif

clean:
	rm mini
