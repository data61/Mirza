import * as React from "react";
import { Link } from "react-router-dom";

import { AuthState } from "../auth";
import { Organisation } from "../business-registry";
import { LabelField } from "./epcis/common";

const { DigitalLink } = require("digital-link.js");

export interface SubmitProps {
  authState: AuthState;
  organisation: Organisation;
  scanData: string;
}

const DigitalLinkPrefix = "https://scan.mirza.d61.io/";

export function Scan(props: SubmitProps) {
  let epc = "";
  let errorStr: string = null;

  const dlUrl = DigitalLinkPrefix + props.scanData;

  try {
    const dl = DigitalLink(dlUrl);

    if (!dl.isValid()) {
      errorStr = "The scanned QR code is not a valid GS1 DigitalLink: " + dlUrl;
    } else {
      epc = dl.mapToGS1Urn();
      if (!epc) {
        errorStr = "TODO: mapToGS1Urn('" + dlUrl + "')";
        epc = "";
      }
    }
  } catch (e) {
    errorStr = "The scanned QR code is not a valid GS1 DigitalLink: " + dlUrl + ". " + e;
  }

  const [error, setError] = React.useState(errorStr);
  const labelState = React.useState(epc);
  const [label, setLabel] = labelState;

  return (
    <section>
      <div className="border-bottom">
        <div className="container">
          <div className="row">
            <div className="column">
              <h3><Link to="/"><i className="fa fa-chevron-left"></i> </Link> Scan DigitalLink</h3>
            </div>
          </div>
        </div>
      </div>
      <div className="container">
        <LabelField state={labelState}
          updateFn={(_, v) => { setError(null); return v; }}
          getFn={(e) => e}
        />
        <div className="error">{error}</div>
        <hr />
        <div className="container">
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
      </div>
    </section>
  );
}
