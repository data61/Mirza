import * as React from 'react';
import * as ReactDOM from 'react-dom';

import { authInit, logIn } from './auth';
import { EventLog } from './components/eventLog';
import { Footer } from './components/footer';
import { Header } from './components/header';
import { MyScanner } from './components/scanner';

authInit().then( (authState) => {
  if (authState === null) {
    logIn();
    return;
  }

  const appState = {
    auth: authState,
  };

  ReactDOM.render(
    <React.Fragment>
      <Header auth={appState.auth}></Header>
      <EventLog></EventLog>
      <MyScanner></MyScanner>
      <Footer></Footer>
    </React.Fragment>,
    document.querySelector('main'));
});
