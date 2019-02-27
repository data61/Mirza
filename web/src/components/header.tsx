import * as React from "react";

export interface HeaderProps { }

export class Header extends React.Component<HeaderProps, {}> {
  render() {
    return <header>
      <div className="container">
        <div className="row">
          <div className="column"><h2>Mirza</h2></div>
          <div className="column align-right">paul</div>
        </div>
      </div>
    </header>
  }
}
