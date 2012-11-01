MAN = scmtap.3
PDF = scmtap.pdf
HTML = scmtap.html

COMMONOPTIONS = --self-contained --number-sections --normalize --smart --toc

TEX_META = -V date='\today' --highlight-style=monochrome
HTML_META = -c manual.css

SCMTAP_TEX_META = -V author="Frank Terbeck"
SCMTAP_TEX_META += -V title="scmtap(3) Manual"
SCMTAP_HTML_META = $(SCMTAP_TEX_META) --highlight-style=monochrome
SCMTAP_MAN_META = -V title="scmtap" -V section="3" -V date="Nov, 2012"

all: man html pdf
man: $(MAN)
pdf: $(PDF)
html: $(HTML)

scmtap.3: scmtap.mdwn
	pandoc -t man $(COMMONOPTIONS) $(SCMTAP_MAN_META) -o $@ scmtap.mdwn
scmtap.pdf: scmtap.mdwn
	pandoc --template template.latex $(COMMONOPTIONS) $(TEX_META) $(SCMTAP_TEX_META) -o $@ scmtap.mdwn
scmtap.html: scmtap.mdwn
	pandoc -t html5 $(HTML_META) $(COMMONOPTIONS) $(SCMTAP_HTML_META) -o $@ scmtap.mdwn

clean:
	rm -f $(HTML) $(PDF) $(MAN) *~

.PHONY: all man html pdf clean
