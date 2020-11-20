from flask import flask
app = flask(__name__)


@app.route("/")
def home():
    return "Hello, flask!"