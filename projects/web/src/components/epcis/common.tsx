import * as React from "react";
import * as QrReader from "react-qr-reader";

import { EventEPCIS } from "../../epcis";
const { DigitalLink } = require("digital-link.js");

export interface EventStateProps<T> {
  state: [T, React.Dispatch<React.SetStateAction<T>>];
}
interface EPCISLabelFieldProps<T> {
  getFn: (e: T) => string;
  updateFn: (e: T, value: string) => T;
}

export function LabelField<T>(
  { state: [getState, setState], updateFn, getFn }: EventStateProps<T> & EPCISLabelFieldProps<T>) {

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

      setState(updateFn(getState, epc));
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
            onChange={(e) => setState(updateFn(getState, e.target.value))}
            value={getFn(getState)}
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
