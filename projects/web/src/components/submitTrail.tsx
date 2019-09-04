import * as React from "react";
import { Link } from "react-router-dom";

import { myGlobals } from "../globals";

import { AuthState } from "../auth";
import { Organisation } from "../business-registry";

export interface QueryProps {
  authState: AuthState;
  organisation: Organisation;
}

export function SubmitTrail(props: QueryProps) {
  const trailEntry = [
    {
      "version": 1,
      "timestamp": "2016-07-22T00:00:00Z",
      "org": "000000",
      "event_id": "00000000-0000-0000-0000-000000000000",
      "previous_signatures": [] as string[],
      "signature": "todo"
    }];

  const submitTrail = () => {

    // TODO: The URL here needs to be changed to props.organisation.url, but the
    // EDAPI that this is currently building against doesn't support the trails
    // service yet.
    return fetch('http://localhost:8300' + '/trail', {
      method: 'POST',
      body: JSON.stringify(trailEntry),
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
        'Authorization': 'Bearer ' + props.authState.getToken().idToken,
      },
    }).then(function(res: Response) {
      if (res.status === 200) {
        alert('Success!');
        window.location.href = 'submit';
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
          <h3><Link to="/"><i className="fa fa-chevron-left"></i> </Link></h3>
        </div>
      </div>
        <div className="border-bottom pad-tb">
          <div className="container">
            <h3>New Trail Entry</h3>
            <div className="row">
              <div className="column border-right">
                <button onClick={submitTrail}>Submit Trail Entry</button>
              </div>
              <div className="column">
                <label>Raw Trail Entry</label>
                <textarea style={({ height: "20em" })} value={JSON.stringify(trailEntry, null, 2)} readOnly></textarea>
              </div>
            </div>
          </div>
        </div>
    </section>
  );
}
