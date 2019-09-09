import * as React from "react";
import { Link } from "react-router-dom";

import { myGlobals } from "../globals";
import { UnsignedTrailEntry, SignedTrailEntry } from "../trails";

import { AuthState } from "../auth";
import { Organisation } from "../org-registry";

import { Redirect } from "react-router-dom";


export interface QueryProps {
  authState: AuthState;
  organisation: Organisation;
  routeProps?: any;
}

// Todo: This should be replaced with a canonical JSON representation and signed using the orgs private key, for now we
//       simulate just by using a one way hash to save implementation effort.
// The following code to produce the SHA256 hex bytes from a string (converstion from canonicalEntry to
//       signatureHexString) is adapted from the example at:
//       https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest
//       and appears to be licensed under public domain (CC0) https://developer.mozilla.org/en-US/docs/MDN/About.
async function signTrailEntry(unsignedTrailEntry : UnsignedTrailEntry) : Promise<SignedTrailEntry> {
  const canonicalEntry = JSON.stringify(unsignedTrailEntry);
  const utf8 = new TextEncoder().encode(canonicalEntry);
  const rawSignature = await crypto.subtle.digest("SHA-256", utf8);
  const signatureHexString = Array.from(new Uint8Array(rawSignature)).map(byte => byte.toString(16).padStart(2, '0')).join('');

  return {...unsignedTrailEntry, signature:signatureHexString};
}

export function SubmitTrail(props: QueryProps) {
  const [nextPreviousSignature, setNextPreviousSignature] = React.useState(null);

  const eventSubmissionTrailData = props.routeProps                                         == null ||
                                   props.routeProps.location                                == null ||
                                   props.routeProps.location.state                          == null ||
                                   props.routeProps.location.state.eventSubmissionTrailData == null ?
                                   [] : props.routeProps.location.state.eventSubmissionTrailData;

  // If we don't have event data redirect to the event submission.
  if ((eventSubmissionTrailData         == null) ||
      (eventSubmissionTrailData.eventId == null)) {
    return <Redirect to='/submit'/>
  }

  // If we have submitted the trail data redirect to the event submission and persist the useful trail state.
  if (nextPreviousSignature !== null) {
    return <Redirect to={{
        pathname: '/submit',
        state: { previousSignature: nextPreviousSignature }
      }}
    />
  }

  const previousSignatures = eventSubmissionTrailData.previousSignatures == null ? [] : eventSubmissionTrailData.previousSignatures;
  const newUnsignedTrailEntry = {
    "version": 1,
    "timestamp": (new Date).toISOString(),
    "org": props.organisation.companyPrefix,
    "event_id": eventSubmissionTrailData.eventId,
    "previous_signatures": previousSignatures as string[]
  };

  const submitTrail = () => {
    // TODO: The URL here needs to be changed to props.organisation.url, but the
    // EDAPI that this is currently building against doesn't support the trails
    // service yet.
    return  signTrailEntry(newUnsignedTrailEntry).then((trailEntry) => {
      return fetch('http://localhost:8300' + '/trail', {
        method: 'POST',
        body: JSON.stringify([trailEntry]),
        headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json',
          'Authorization': 'Bearer ' + props.authState.getToken().idToken,
        }
      }).then(function(res: Response) {
        if (res.status === 200) {
          alert('Success!');

          const previousSignature = trailEntry.signature;
          setNextPreviousSignature(previousSignature);
        } else {
          alert('Failed with status: ' + res.status);
        }
      });
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
                <textarea style={({ height: "20em" })} value={JSON.stringify(newUnsignedTrailEntry, null, 2)} readOnly></textarea>
              </div>
            </div>
          </div>
        </div>
    </section>
  );
}
