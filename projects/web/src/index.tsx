import * as React from "react";
import * as ReactDOM from "react-dom";
import { BrowserRouter, Route, Switch } from "react-router-dom";

import { EventLog } from "./components/eventLog";
import { Footer } from "./components/footer";
import { Header } from "./components/header";
import { Home } from "./components/home";
import { NotFound } from "./components/notFound";
import { Submit } from "./components/submit";

import { authInit, logIn } from "./auth";
import { BusinessRegistry } from "./business-registry";

authInit().then((authState) => {
  if (authState === null) {
    logIn();
    return;
  }

  const br = new BusinessRegistry(authState.getToken());

  br.getOrganisations().then((orgs) => {
    if (orgs.length < 1) {
      ReactDOM.render(<div>You need to be a member of an organisation</div>, document.querySelector("main"));
      return;
    }

    const appState = {
      auth: authState,
      organisation: orgs[0],
    };

    ReactDOM.render(<BrowserRouter>
      <React.Fragment>
        <Header auth={appState.auth} organisation={appState.organisation}></Header>
        <Switch>
          <Route path="/" exact component={Home} />
          <Route path="/events" exact render={() =>
            <EventLog authState={appState.auth} organisation={appState.organisation}></EventLog>} />
          <Route path="/scan" exact render={() =>
            <Submit authState={appState.auth} organisation={appState.organisation} ></Submit>} />
          <Route component={NotFound} />
        </Switch>
        <Footer></Footer>
      </React.Fragment>
    </BrowserRouter>, document.querySelector("main"));
  });
});
