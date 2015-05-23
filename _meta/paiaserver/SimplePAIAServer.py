"""Simple PAIA Server. based on SimpleHTTPServer

"""



__all__ = ["SimplePAIAHandler"]

import BaseHTTPServer
import urllib
import urlparse
import posixpath
import json
import cgi
from StringIO import StringIO


import SimplePAIAConfig

def enum(*sequential, **named):
    enums = dict(zip(sequential, range(len(sequential))), **named)
    return type('Enum', (), enums)

   
class SimplePAIAHandler(BaseHTTPServer.BaseHTTPRequestHandler):

    """.Simple PAIA Server

        Like BaseHTTPRequestHandler it will handle the connection stuff/PAIA API and then call 
        a corresponding method do_something (e.g.  do_auth_login, do_core_items, ...) when an request occurs.
        The return value of these methods is serialized to json and send as response. 
        
        The attribute query_vars contains the GET/POST variables of the request.
        The method paia_error can be used to create an error response.
        
        Configuration is in SimplePAIAConfig.py, see paiatestserver.py for an example usage
    """
            
    Status = enum("NORELATION", "RESERVED", "ORDERED", "HELD", "PROVIDED", "REJECTED")
    identified_patrons = {}

    def __init__(self, request, client_address, server):
        self.query_vars = {}
        BaseHTTPServer.BaseHTTPRequestHandler.__init__(self, request, client_address, server);
    

    def do_GET(self):      
        self.docommon()

    def do_HEAD(self):
        self.docommon()

    def do_POST(self): 
        postVars = {}     
        print(self.path)
        if not("?" in self.path):
            ctype, pdict = cgi.parse_header(self.headers['content-type'])
            if ctype == 'multipart/form-data':
                postVars = cgi.parse_multipart(self.rfile, pdict)
            elif ctype == 'application/x-www-form-urlencoded':
                length = int(self.headers['content-length'])
                postVars = urlparse.parse_qs(self.rfile.read(length), keep_blank_values=1)
        print(postVars)
         
        self.docommon(postVars)

    def docommon(self, postVars = {}):
        """ ... """
        spath = urlparse.urlparse("/" + self.path.lstrip("/"))
        path = posixpath.normpath(urllib.unquote(spath.path))
        self.query_vars = dict(urlparse.parse_qs(spath.query).items() + postVars.items())

        if path == "/auth/login": 
            self.send_response(200)
            self.send_header("Content-type", "application/json")
            self.end_headers()
            r = self.do_auth_login();
            if "access_token" in r:
                if "patron" in r: patron_id = r["patron"]
                else: patron_id = self.query_vars["username"][0]
                print(patron_id)
                self.identified_patrons[patron_id] = {"access_token": r["access_token"]}
            
            self.send_json(r);
            return
        
        if path.startswith("/core/"):
            words = filter(None, path.split('/'))
            if SimplePAIAConfig.config.get("check_access_token", True): 
                if not (words[1] in self.identified_patrons): 
                    self.send_error(500, "No login for patron occured ")
                    return None
                if self.query_vars["access_token"][0] != self.identified_patrons[words[1]]["access_token"]:
                    self.send_error(500, "invalid token")
                    return None
            self.send_response(200)
            self.send_header("Content-type", "application/json")
            self.end_headers()
            if words[2] == "items": r = self.do_core_items(words[1])
            elif words[2] == "renew": 
                length = int(self.headers['content-length'])
                r = self.do_core_renew(words[1], json.loads(self.rfile.read(length)))
            else: 
                self.send_error(500, "invalid action")
                return None
            self.send_json(r)
            return
       
        self.send_error(404, "Unknown url")
        return None


    def send_json(self, data):
        """Sends Python data, as JSON """
        if SimplePAIAConfig.config.get("print_request", False):
            print(self.path)
            print("=>")
            print(json.dumps(data))
            print("");
        json.dump(data, self.wfile)

    def paia_error(self, code, message):
        return {"error": {"code": code, "message": message}}

    def do_auth_login(self):
        return self.paia_error("?", "not implemented");
    def do_core_items(self, patron):
        return self.paia_error("?", "not implemented");
    def do_core_renew(self, patron, data):
        return self.paia_error("?", "not implemented");


def test(HandlerClass = SimplePAIAHandler,
         ServerClass = BaseHTTPServer.HTTPServer):
    BaseHTTPServer.test(HandlerClass, ServerClass)


if __name__ == '__main__':
    test()


