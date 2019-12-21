Tiny Tags is a RESTful web service providing short unique identifiers.

This project is a proof of concept for how to write a web service in
Common Lisp.  The following Common Lisp packages are used.

   * uiop: Operating system interface.
   * iterate: Don't loop, iterate.
   * sqlite, cl-dbi: SQL database interface.
   * hunchentoot: Web server.
   * html-template: Page generation.
   * snooze: Define REST handlers.
   * cl-json: Response formatting.

The web service has a landing page so that the user can interact
with the service.  The contents and style of the landing page is
fully customizable through an external HTML template.

The web service is also accessible programmatically through a REST
API.  After checking out various light and heavy web frameworks I
finally decided to use Snooze.  Main reason for this decision was
the simple parameter handling.

Finally, the service can be configured to redirect all HTTP requests
to HTTPS.  You have to provide the SSL certificate (the GNUmakefile
contains a rule to generate a self-signed certificate).

A screen shot of the landing page can he found
[here](images/Tiny_Tag_Web_Service.pdf).
