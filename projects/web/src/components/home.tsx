import * as React from "react";
import { Link } from "react-router-dom";

import { AuthState } from "../auth";
import { Organisation } from "../business-registry";

export interface HomeProps {
  authState: AuthState;
  organisation: Organisation;
}

export function Home({ authState, organisation }: HomeProps) {
  return (
    <section>
      <div className="container">
        <h2>Welcome to the Mirza platform.</h2>
        <p>Mirza is a web platform for capturing traceability events in a supply chain.</p>

        <h4>Standards</h4>

        <blockquote>
          EPCIS is a GS1 standard that enables trading partners to share information about the
          physical movement and status of products as they travel throughout the supply chain –
          from business to business and ultimately to consumers. It helps answer the “what, where,
          when and why” questions to meet consumer and regulatory demands for accurate and
          detailed product information.
        </blockquote>

        <h4>Resources</h4>
        <ul>
          <li><a href="https://www.gs1.org/standards/epcis">ECPIS and Core Business Vocabulary (CBV)</a></li>
          <li>
            <a href="https://www.gs1.org/standards/traceability/traceability/2-0">GS1 Global Traceability Standard</a>
          </li>
        </ul>
      </div>
      <hr />
      <div className="container">
        <h3>Federation</h3>
        <p>
          You are signed in as <strong>{authState.getName()}</strong>,
          an employee of the <strong>{organisation.name}</strong> organisation.
          The EPCIS repository which you will submit and query events is at <strong>{organisation.url}</strong>.
        </p>
      </div>
      <hr />
      <div className="container">
        <h3>Actions</h3>
        <div className="row buttonBar">
          <Link className="largeButton" to="/submit">
            <i className="fas fa-truck-loading fa-4x"></i>
            <hr />
            Submit Event
          </Link>
          <Link className="largeButton" to="/events">
            <i className="fas fa-search fa-4x"></i>
            <hr />
            Event Log
          </Link>
        </div>
      </div>
    </section>
  );
}
