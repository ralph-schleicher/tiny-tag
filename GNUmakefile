## GNUmakefile --- build rules for the tiny tag web service.

# Copyright (C) 2019 Ralph Schleicher

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#    * Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#
#    * Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in
#      the documentation and/or other materials provided with the
#      distribution.
#
#    * Neither the name of the copyright holder nor the names of its
#      contributors may be used to endorse or promote products derived
#      from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

## Code:

# Parameters for the SSL certificate.
#
# Host name.
host := $(shell hostname -s)
# Domain name.
domain := $(shell hostname -d)
# Fully qualified host name.
full := $(host).$(domain)
# IP address.
addr = $(shell echo `hostname -I` | tr ' ' '\n' | grep -v ':')

.PHONY: all
all: html css ssl database

### HTML Template

.PHONY: html
html: index.html.in
# There are two options.  Either
#
#      <link rel="stylesheet" type="text/css" href="style.css.min" />
# or
#      <style type="text/css">
#       ...
#      </style>
#
# When embedding the CSS style sheet, indent the code properly.
#
# Also remove all empty lines in the HTML template.
index.html.in: index.html.in.in style.css.min
	{ \
	  echo '<style type="text/css">' ; \
	  cat style.css.min | sed -e 's/^/ /' ; \
	  echo '</style>' ; \
	} | \
	sed -e 's/^/  /' > temp.css
	sed -e '/<!-- TMPL_VAR CSS -->/ {' \
	    -e 'r temp.css' \
	    -e 'd' \
	    -e '}' \
	    -e '/^$$/d' \
	index.html.in.in > $@~
	rm -f temp.css
	mv -f $@~ $@

### CSS Style Sheet

.PHONY: css
css: style.css.min style.css
# Add a final newline character to the compressed style sheet.
# The sed(1) script joins lines of the form
#
#      @media ...{...{...}
#      }
style.css.min: style.css.in
	lessc --clean-css='--s1 -b' $< $@~
	echo >> $@~
	sed -i -e ':x /}$$/ { N; s/}\n}/}}/; bx }' $@~
	mv -f $@~ $@

# Ditto without compression.
style.css: style.css.in
	lessc $< $@

### SSL Certificate

.PHONY: ssl
ssl: example.crt example.key
example.crt: ssl.stamp
example.key: ssl.stamp
ssl.stamp:
	openssl req -x509 -newkey rsa:4096 -sha256 -days 3650 \
	-nodes -keyout example.key -out example.crt \
	-subj /CN=$(full) \
	-addext subjectAltName=DNS:$(full),DNS:$(domain),DNS:localhost,IP:$(addr),IP:127.0.0.1
	echo timestamp > ssl.stamp

### Database

.PHONY: database
database: tiny-tag.sqlite
# In concurrent environments the write-ahead log generates less
# denials, i.e. busy timeouts, than the default rollback journal.
tiny-tag.sqlite:
	sqlite3 $@ '.read tiny-tag.sql'
	sqlite3 $@ 'pragma journal_mode=wal'

### Maintenance

README.html: README.md
	markdown $< > $@~ && mv -f $@~ $@

.PHONY: sync
sync: all README.html
	~/src/github/github.sh tiny-tag

## GNUmakefile ends here
