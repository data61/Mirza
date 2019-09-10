import * as React from "react";
import { Link } from "react-router-dom";

import { AuthState } from "../auth";
import { Organisation } from "../org-registry";
import { objectEvent } from "../epcis";
import { EventForm } from "./epcis/event";
import { Redirect } from "react-router-dom";

export interface SubmitProps {
  authState: AuthState;
  organisation: Organisation;
  routeProps?: any;
}

export function Submit(props: SubmitProps) {
  const eventState = React.useState(objectEvent());
  const [event, _] = eventState;

  const [trailData, setTrailData] = React.useState(null);

  if (trailData !== null) {
    return <Redirect to={{
      pathname: '/submitTrail',
      state: {eventSubmissionTrailData : trailData }
    }}
    />
  }

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
        alert('Successfully submited!');
        return res.json();
      } else {
        alert('Failed with status: ' + res.status);
      }

      throw res;
    }).then(function(resultData) {
      if ((!Array.isArray(resultData)) ||
          (resultData.length < 2)) {
        throw resultData;
      }
      const eventId = resultData[1];

      const previousSignatures = props.routeProps                                  == null ||
                                 props.routeProps.location                         == null ||
                                 props.routeProps.location.state                   == null ||
                                 props.routeProps.location.state.previousSignature == null ?
                                 [] : [props.routeProps.location.state.previousSignature];
      const trailData = {
        "eventId": eventId,
        "previousSignatures": previousSignatures
      };
      setTrailData(trailData);

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
              <h3><Link to="/"><i className="fa fa-chevron-left"></i> </Link></h3>
            </div>
          </div>
        </div>
      </div>
      <div>
        <div className="border-bottom pad-tb">
          <div className="container">
            <h3>New Event</h3>
            <div className="row">
              <div className="column border-right">
                <EventForm state={eventState} />

                <button onClick={submitEvent}>Submit Event</button>
              </div>
              <div className="column">
                <label>Raw EPCIS Event</label>
                <textarea style={({ height: "20em" })} value={JSON.stringify(event, null, 2)} readOnly></textarea>
              </div>
            </div>
          </div>
        </div>
      </div >
    </section >
  );
}
