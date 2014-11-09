"""Test PAIA Server """



__all__ = ["PAIATestHandler"]

import BaseHTTPServer
import SimplePAIAServer
import json

Status = SimplePAIAServer.SimplePAIAHandler.Status

class PAIATestHandler(SimplePAIAServer.SimplePAIAHandler):
    """ PAIA Server  """

    data = {
      "test": {
        "password": "12345",
        "books": [
          {"label": "b1r", "about": "test book 1", "duedate": "2014-11-12", "status": Status.HELD, "canrenew": True},
          {"label": "b2r", "about": "test book 2", "duedate": "2014-12-03", "status": Status.HELD, "canrenew": True},
          {"label": "b3", "about": "test book 3", "duedate": "2014-12-03", "status": Status.HELD, "canrenew": False},
          {"label": "b4", "about": "test book 4", "duedate": "2014-12-05", "status": Status.HELD, "canrenew": False},
        ]
      },
      "stdin": {
        "password": "p"
        #no books, pass [{"status": 3, "canrenew": true, "about": "test book 1", "duedate": "2014-11-12", "label": "b1r"}, {"status": 3, "canrenew": true, "about": "test book 2", "duedate": "2014-12-03", "label": "b2r"}, {"status": 3, "canrenew": false, "about": "test book 3", "duedate": "2014-12-03", "label": "b3"}, {"status": 3, "canrenew": false, "about": "test book 4", "duedate": "2014-12-05", "label": "b4"}] on the command line
      }
    }

    def do_auth_login(self):
        username = self.query_vars["username"][0]
        password = self.query_vars["password"][0]
        if not (username in self.data): 
          return self.paia_error("?", "unknown user");
        user = self.data[username]
        if user["password"] != password:
          return self.paia_error("?", "invalid password");
        r = {"access_token": user["password"], "patron": username}
        if "scope" in user: r["scope"] = user["scope"]
        return r;
        
    def do_core_items(self, patron):
        if patron != "stdin":
            return {"doc": self.data[patron]["books"]};
        return {"doc": json.loads(raw_input("Need PAIA JSON for user books: "))};
#    def do_core_renew(self, patron):
#        return {"doc": self.data[patron].books};


def test(HandlerClass = PAIATestHandler,
         ServerClass = BaseHTTPServer.HTTPServer):
    BaseHTTPServer.test(HandlerClass, ServerClass)


if __name__ == '__main__':
    test()


