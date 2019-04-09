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

  br.getBusiness().then((business) => {

    const appState = {
      auth: authState,
      business,
    };

    ReactDOM.render(<BrowserRouter>
      <React.Fragment>
        <Header auth={appState.auth} business={appState.business}></Header>
        <Switch>
          <Route path="/" exact component={Home} />
          <Route path="/events" exact component={EventLog} />
          <Route path="/scan" exact component={Submit} />
          <Route component={NotFound} />
        </Switch>
        <Footer></Footer>
      </React.Fragment>
    </BrowserRouter>, document.querySelector("main"));
  });
});
