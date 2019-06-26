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
              <h3><Link to="/"><i className="fa fa-chevron-left"></i> </Link></h3>
            </div>
          </div>
        </div>
      </div>
      <div className="border-bottom pad-tb">
        <div className="container">
          <h3>Scan GS1 DigitalLink</h3>
          <LabelField state={labelState}
            updateFn={(_, v) => { setError(null); return v; }}
            getFn={(e) => e}
          />
          <div className="error">{error}</div>
          <hr />
          <h3>Actions</h3>
          <div className="row buttonBar">
            <Link className="largeButton" to={"/actions/pack/" + encodeURIComponent(label)}>
              <i className="fas fa-truck fa-4x"></i>
              <hr />
              Pack
            </Link>
          </div>

        </div>
      </div>
    </section>
  );
}
