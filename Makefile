kiss: kiss.go
ifdef VERSION
	go build -ldflags="-X 'main.version=$(VERSION)'" -o kiss .
else
	go build -o kiss .
endif

clean:
	rm kiss
