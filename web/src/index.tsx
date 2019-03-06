import * as React from "react";
import * as ReactDOM from "react-dom";

import { Header } from "./components/header";
import { Footer } from "./components/footer";
import { EventLog } from "./components/eventLog";
import { authInit, logIn } from "./auth";

authInit().then(authState => {
  if (authState === null) {
    logIn();
    return;
  }

  const appState = {
    auth: authState
  }

  ReactDOM.render(<React.Fragment>
    <Header auth={appState.auth}></Header>
    <EventLog></EventLog>
    <Footer></Footer>
  </React.Fragment>, document.querySelector("main"));
})
