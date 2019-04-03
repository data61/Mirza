import * as React from "react";
import { AuthState, logOut } from "../auth";

export interface HeaderProps {
  auth: AuthState;
}

export function Header(props: HeaderProps) {
  return <header>
    <div className="container">
      <div className="row">
        <div className="column"><h2>Mirza</h2></div>
        <div className="column">
          <div className="flex-end-middle">
            <span>{props.auth.getName()}</span>
            <a onClick={logOut} className="button button-clear inline">( Log out )</a>
            <img className="round" height="48px" src={props.auth.getPictureUrl()} />
          </div>
        </div>
      </div>
    </div>
  </header>;
}
