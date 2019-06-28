import * as React from "react";
import { Link } from "react-router-dom";

import { AuthState } from "../../auth";
import { Organisation } from "../../business-registry";
import { EventBusinessStep, EventDisposition, transactionEvent } from "../../epcis";
import { LabelField } from "../epcis/common";

export interface PackProps {
  authState: AuthState;
  organisation: Organisation;
  label: string;
}

const DigitalLinkPrefix = "https://scan.mirza.d61.io/";

export function Pack(props: PackProps) {
  const ev = transactionEvent();
  ev.bizStep = EventBusinessStep.Loading;
  ev.disposition = EventDisposition.InTransit;
  ev.epcList[0] = props.label;

  const eventState = React.useState(ev);

  const [event, _] = eventState;
  const [submitted, setSubmitted] = React.useState(false);

  const submitEvent = () => {

    return fetch(props.organisation.url + '/event', {
      method: 'POST',
      body: JSON.stringify(event),
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
        'Authorization': 'Bearer ' + props.authState.getToken().idToken,
      },
    }).then(function(res: Response) {
      if (res.status === 200) {
        setSubmitted(true);
      } else {
        alert('Failed with status: ' + res.status);
      }
    }).catch(function(err) {
      console.log(err);
    });
  };

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
          <h3>Ship Package</h3>
          <hr />
          <div className="row">
            <div className="column column-20">
              <i className="fas fa-box fa-4x"></i>
            </div>
            <div className="column column-80">
              <LabelField text="Shipping Container Code (SSCC)"
                state={eventState}
                updateFn={(e, v) => { e.epcList[0] = v; return e; }}
                getFn={(e) => e.epcList[0]} />
            </div>
          </div>
          <hr />
          <div className="row">
            <div className="column column-20">
              <i className="fas fa-truck fa-4x"></i>
            </div>
            <div className="column column-80">
              <LabelField text="Vehicle Identifier (GIAI)"
                state={eventState}
                updateFn={(e, v) => { e.parentID = v; return e; }}
                getFn={(e) => e.parentID} />
            </div>
          </div>
          <hr />
          {!submitted ? (
            <div className="row buttonBar">
              <button onClick={submitEvent}>
                <i className="fas fa-clipboard-check fa-lg fa-fw"></i> Submit Event
              </button>
            </div>
          ) : (<div className="row">Submitted.</div>)}
        </div>
      </div>
    </section >
  );
}
