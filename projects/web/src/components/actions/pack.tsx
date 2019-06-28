import * as React from "react";
import { Link } from "react-router-dom";

import { AuthState } from "../../auth";
import { Organisation } from "../../business-registry";
import { aggregationEvent } from "../../epcis";
import { LabelField } from "../epcis/common";

export interface PackProps {
  authState: AuthState;
  organisation: Organisation;
  label: string;
}

const DigitalLinkPrefix = "https://scan.mirza.d61.io/";

export function Pack(props: PackProps) {
  const e = aggregationEvent();
  e.epcList[0] = props.label;

  const eventState = React.useState(e);

  const [event, _] = eventState;

  return (
    <section>
      <div className="border-bottom">
        <div className="container">
          <div className="row">
            <div className="column">
              <h3><Link to="/"><i className="fa fa-chevron-left"></i></Link></h3>
            </div>
          </div>
        </div>
      </div>
      <div className="border-bottom pad-tb">
        <div className="container">
          <h3>Pack</h3>

          <LabelField state={eventState}
            updateFn={(e, v) => e}
            getFn={(e) => e.epcList[0]}
          />

        </div>
      </div>
    </section>
  );
}
