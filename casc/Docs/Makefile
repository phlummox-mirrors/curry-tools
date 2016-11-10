default: curry-style-guide.html

# Generate an HTML version of the style guide.
curry-style-guide.html: curry-style-guide.md style.html
	pandoc -s -N --toc -H style.html -o $@ $<
