import * as React from "react";
import * as ReactDOM from "react-dom";

import { Header } from "./components/header";
import { Footer } from "./components/footer";

ReactDOM.render(<React.Fragment>
  <Header></Header>
  <section>COntent</section>
  <Footer></Footer>
</React.Fragment>, document.querySelector("main"));
