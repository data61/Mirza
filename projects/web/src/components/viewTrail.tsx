import * as React from "react";
import { Link } from "react-router-dom";

import { AuthState } from "../auth";
import { Organisation } from "../org-registry";

import { isSignedTrailEntryArray} from "../trails";


export interface SubmitProps {
  authState: AuthState;
  organisation: Organisation;
}

export function ViewTrail(props: SubmitProps) {
  const [signaturesText, setSignaturesText] = React.useState("");
  const [trail, setTrail] = React.useState([]);

  const retrieveTrail = () => {
    return fetch(props.organisation.url + '/trails/trail/' + signaturesText, {
      method: 'GET',
      headers: {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
        'Authorization': 'Bearer ' + props.authState.getToken().idToken,
      },
    }).then(function(res: Response) {
      if (res.status === 200) {
        return res.json();
      } else {
        alert('Failed with status: ' + res.status);
      }

      throw res;
    }).then(function(resultData) {
      if (!isSignedTrailEntryArray(resultData)) {
        throw resultData;
      }

      setTrail(resultData);
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
            <h3>View Trail</h3>
            <div className="row">
              <div className="column border-right">
                <label>Trail Entry Identifier (Signature or Event Id)
                  <input type="text"
                         placeholder="Trail Entry Signature or Event Id"
                         value={signaturesText}
                         onChange={(e) => setSignaturesText(e.target.value)}
                  />
                </label>
                <button onClick={retrieveTrail}>Retrieve Trail</button>
              </div>
            </div>
            <div className="row">
              <div>
                <table>
                  <thead>
                    <tr>
                      <th>Signature</th>
                      <th>Timestamp</th>
                      <th>Event Id</th>
                      <th>Org Identifier</th>
                      <th>Previous Signatures</th>
                    </tr>
                  </thead>
                  <tbody>{trail.map(function(item, key) {
                    return (
                      <tr key = {item.signature}>
                        <td>{item.signature}</td>
                        <td>{item.timestamp}</td>
                        <td>{item.event_id}</td>
                        <td>{item.org}</td>
                        <td>{item.previous_signatures.join(", ")}</td>
                      </tr>
                    )
                  })}</tbody>
                </table>
              </div>
            </div>
          </div>
        </div>
      </div >
    </section >
  );
}
