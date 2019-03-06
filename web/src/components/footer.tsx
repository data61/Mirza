import * as React from "react";

export interface FooterProps { }

export class Footer extends React.Component<FooterProps, {}> {
  render() {
    return <footer>
      <div className="container align-center">Copyright &copy; 2019 CSIRO Data61</div>
    </footer>
  }
}
