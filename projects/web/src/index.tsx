import * as React from "react";
import * as ReactDOM from "react-dom";
import { BrowserRouter, Route, Switch } from "react-router-dom";

import { Pack } from "./components/actions/pack";
import { EventLog } from "./components/eventLog";
import { Footer } from "./components/footer";
import { Header } from "./components/header";
import { Home } from "./components/home";
import { NotFound } from "./components/notFound";
import { Scan } from "./components/scan";
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
          <Route path="/" exact render={() =>
            <Home authState={appState.auth} organisation={appState.organisation}></Home>} />

          <Route path="/events" exact render={() =>
            <EventLog authState={appState.auth} organisation={appState.organisation}></EventLog>} />

          <Route path="/submit" exact render={() =>
            <Submit authState={appState.auth} organisation={appState.organisation} ></Submit>} />

          <Route path="/scan/:scanData*" exact render={({ match }) =>
            <Scan authState={appState.auth}
              organisation={appState.organisation}
              scanData={match.params.scanData}></Scan>} />

          <Route path="/actions/pack/:label" exact render={({ match }) =>
            <Pack authState={appState.auth}
              organisation={appState.organisation}
              label={decodeURIComponent(match.params.label)}></Pack>} />

          <Route component={NotFound} />
        </Switch>
        <Footer></Footer>
      </React.Fragment>
    </BrowserRouter>, document.querySelector("main"));
  });
});
