package org.renci.owl.differ

import akka.http.scaladsl.model.MediaTypes
import akka.http.scaladsl.marshalling.Marshaller
import akka.http.scaladsl.marshalling.ToEntityMarshaller

object Home {

  def homeForm: HTMLPage = HTMLPage("""
<!DOCTYPE html>
<html>
<head>
	<meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">
	  <title>OWL diff</title>
    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta/css/bootstrap.min.css" integrity="sha384-/Y6pD6FV/Vv2HJnA6t+vslU6fwYXjCFtcEpHbNJ0lyAFsXTsjBbfaDjzALeQsN6M" crossorigin="anonymous">
</head>
<body>
<div class="container-fluid">
    <h2>Basic OWL diff</h2>
    <p>Enter URLs for two versions of an OWL ontology to see a listing of added and removed axioms.</p>
	  <form action="diff">
        <div class="form-group">
			      <label class="form-label" for="left">Previous ontology URL:</label>
			      <input class="form-control" name="left" id="left" placeholder="previous version">
        </div>
        <div class="form-group">
			      <label class="form-label" for="right">New ontology URL:</label>
			      <input class="form-control" name="right" id="right" placeholder="new version">
        </div>
        <div class="form-check">
            <label class="form-check-label" for="loadimports">
                <input class="form-check-input" type="checkbox" name="loadimports" id="loadimports" value="true" checked="checked">
                Load imports
            </label>
        </div>
        <button type="submit" class="btn btn-primary">Submit</button>
	  </form>
    <footer>
        <p><i>Source code for this tool is at <a href="https://github.com/balhoff/owl-diff">github/balhoff/owl-diff</a></i></p>
    </footer>
</div>
</body>
</html>
    """)

}

case class HTMLPage(content: String)

object HTMLPage {

  implicit val HTMLPageMarshaller: ToEntityMarshaller[HTMLPage] = Marshaller.stringMarshaller(MediaTypes.`text/html`).compose(_.content)

}