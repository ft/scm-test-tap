MAN = test-tap.3
PDF = test-tap.pdf
HTML = test-tap.html
INFO = test-tap.info

COMMONOPTIONS = --self-contained --number-sections --toc

TEX_META = -V date='\today' --highlight-style=monochrome
HTML_META = -c manual.css

SCMTAP_TEX_META = -V author="Frank Terbeck"
SCMTAP_TEX_META += -V title="test-tap(3) Manual"
SCMTAP_HTML_META = $(SCMTAP_TEX_META) --highlight-style=monochrome
SCMTAP_MAN_META = -V title="test-tap" -V section="3" -V date="Nov, 2012"

all: man html pdf info
man: $(MAN)
pdf: $(PDF)
html: $(HTML)
info: $(INFO)

test-tap.3: test-tap.mdwn
	pandoc -t man $(COMMONOPTIONS) $(SCMTAP_MAN_META) -o $@ test-tap.mdwn
test-tap.pdf: test-tap.mdwn
	pandoc --template template.latex $(COMMONOPTIONS) $(TEX_META) $(SCMTAP_TEX_META) -o $@ test-tap.mdwn
test-tap.html: test-tap.mdwn
	pandoc -t html5 $(HTML_META) $(COMMONOPTIONS) $(SCMTAP_HTML_META) -o $@ test-tap.mdwn
test-tap.texi: test-tap.mdwn
	pandoc -t texinfo $(INFO_META) $(COMMONOPTIONS) $(SCMTAP_INFO_META) -o $@ test-tap.mdwn
test-tap.info: test-tap.texi
	makeinfo --no-validate --force $< -o $@

clean:
	rm -f $(HTML) $(PDF) $(MAN) *~

.PHONY: all man html pdf clean
