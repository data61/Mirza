import * as React from "react";
import * as ReactDOM from "react-dom";

import { Header } from "./components/header";
import { Footer } from "./components/footer";
import { EventLog } from "./components/eventLog";

ReactDOM.render(<React.Fragment>
  <Header></Header>
  <EventLog></EventLog>
  <Footer></Footer>
</React.Fragment>, document.querySelector("main"));
