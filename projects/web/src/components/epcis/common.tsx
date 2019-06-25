import * as React from "react";
import * as QrReader from "react-qr-reader";

import { Event } from "../../epcis";
const { DigitalLink } = require("digital-link.js");

import { AuthState } from "../../auth";
import { Organisation } from "../../business-registry";

export interface Props {
  authState: AuthState;
  organisation: Organisation;
}

export interface EventStateProps {
  eventState: [Event, React.Dispatch<React.SetStateAction<Event>>];
}
interface EPCISLabelFieldProps {
  getFn: (e: Event) => string;
  updateFn: (e: Event, value: string) => Event;
}

export function LabelField({ eventState: [event, setEvent], updateFn, getFn }: EventStateProps & EPCISLabelFieldProps) {

  const [showQR, setShowQR] = React.useState(false);

  const toggleQR = () => setShowQR(!showQR);
  const onScan = (data?: string) => {
    if (data !== null) {
      const dl = DigitalLink(data);
      if (!dl.isValid()) {
        alert("The scanned QR code is not a valid GS1 DigitalLink");
        return;
      }
      const epc = dl.mapToGS1Urn();
      if (!epc) {
        alert("TODO: mapToGS1Urn('" + data + "')");
        return;
      }

      setEvent(updateFn(event, epc));
      toggleQR();
    }
  };
  const onScanError = () => alert("Unable to scan QR code");

  return (
    <label>
      <span>Label</span>
      <div className="row">
        <div className="column column-90">
          <input type="text"
            placeholder="EPC Label"
            onChange={(e) => setEvent(updateFn(event, e.target.value))}
            value={getFn(event)}
          />
        </div>
        <div className="column column-10">
          <a title="Use QR Code" onClick={toggleQR}><i className="fas fa-fw fa-lg fa-qrcode"></i></a>
        </div>
      </div>

      {showQR &&
        <QrReader delay={300}
          onError={onScanError}
          onScan={onScan}
          showViewFinder={false}
        //          style={{ width: "128px" }}
        />}
    </label>
  );
}

interface Mapping {
  [key: string]: string;
}

export function MappingSelect(props: { mapping: Mapping, value: string, setValue: (value: string) => void }) {
  const f = (s: string) => s.replace(/([a-z0-9])([A-Z])/g, (x: string) => x[0] + " " + x[1]);

  return (
    <select defaultValue={props.value} onChange={(e) => props.setValue(e.target.value)}>
      {Object.keys(props.mapping).map((k: string) => (<option
        key={k}
        value={props.mapping[k]}
      >{f(k)}</option>))}
    </select>
  );
}
