"""Test PAIA Server """



__all__ = ["PAIATestHandler"]

import BaseHTTPServer
import SimplePAIAServer
import json
import datetime
import time

Status = SimplePAIAServer.SimplePAIAHandler.Status

class PAIATestHandler(SimplePAIAServer.SimplePAIAHandler):
    """ PAIA Server  """

    data = {
      "test": {
        "password": "12345",
        "books": [
          {"label": "b1r", "about": "test book 1", "duedate": "2014-11-12", "status": Status.HELD, "canrenew": True, "item": "intid1"},
          {"label": "b2r", "about": "test book 2", "duedate": "2014-12-03", "status": Status.HELD, "canrenew": True, "item": "intid2"},
          {"label": "b3", "about": "test book 3", "duedate": "2014-12-03", "status": Status.HELD, "canrenew": False, "item": "intid3"},
          {"label": "b4", "about": "test book 4", "duedate": "2014-12-05", "status": Status.HELD, "canrenew": False, "item": "intid4"},
        ]
      },
      "stdin": {
        "password": "p"
        #no books, pass [{"status": 3, "canrenew": true, "about": "test book 1", "duedate": "2014-11-12", "label": "b1r"}, {"status": 3, "canrenew": true, "about": "test book 2", "duedate": "2014-12-03", "label": "b2r"}, {"status": 3, "canrenew": false, "about": "test book 3", "duedate": "2014-12-03", "label": "b3"}, {"status": 3, "canrenew": false, "about": "test book 4", "duedate": "2014-12-05", "label": "b4"}] on the command line
      }
    }
    def simulate_lag(self):
        time.sleep(4)

    def do_auth_login(self):
        self.simulate_lag()
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
        self.simulate_lag()
        if patron != "stdin":
            return {"doc": self.data[patron]["books"]};
        return {"doc": json.loads(raw_input("Need PAIA JSON for user books: "))};

    def do_core_renew(self, patron, data):       
        self.simulate_lag()
        books = self.data[patron]["books"]
        if (isinstance(data["doc"], dict)): renewItems = [ data["doc"]["item"] ]
        else: renewItems = [ book["item"] for book in data["doc"] ]
        renewItemPos = [i for i in range(0,len(books)) if books[i]["item"] in renewItems]
        if False in [books[i]["canrenew"] for i in renewItemPos ]:
            return self.paia_error("?", "renewing not allowed")
        newDate = str((datetime.datetime.now()).date()+datetime.timedelta(days=7));
        for i in renewItemPos:
            books[i]["duedate"] = newDate
        return {"doc": [ books[i] for i in renewItemPos ]};


def test(HandlerClass = PAIATestHandler,
         ServerClass = BaseHTTPServer.HTTPServer):
    BaseHTTPServer.test(HandlerClass, ServerClass)


if __name__ == '__main__':
    test()


