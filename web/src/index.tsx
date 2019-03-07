import * as React from "react";
import * as ReactDOM from "react-dom";
import { BrowserRouter, Route, Switch } from "react-router-dom";

import { EventLog } from "./components/eventLog";
import { Footer } from "./components/footer";
import { Header } from "./components/header";
import { Home } from "./components/home";
import { NotFound } from "./components/notFound";
import { MyScanner } from "./components/scanner";

import { authInit, logIn } from "./auth";

authInit().then( (authState) => {
  if (authState === null) {
    logIn();
    return;
  }

  const appState = {
    auth: authState,
  };

  ReactDOM.render(<BrowserRouter>
    <React.Fragment>
      <Header auth={appState.auth}></Header>
      <Switch>
        <Route path="/" exact component={Home} />
        <Route path="/events" exact component={EventLog} />
        <Route path="/scan" exact component={MyScanner} />
        <Route component={NotFound} />
      </Switch>
      <Footer></Footer>
    </React.Fragment>
  </BrowserRouter>, document.querySelector("main"));
});
