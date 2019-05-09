import * as React from "react";
import * as QrReader from "react-qr-reader";

import { Event } from "../../epcis";

export interface EventStateProps {
  eventState: [Event, React.Dispatch<React.SetStateAction<Event>>];
}
interface EPCISLabelFieldProps {
  updateFn: (e: Event, value: string) => Event;
}

export function LabelField({ eventState: [event, setEvent], updateFn }: EventStateProps & EPCISLabelFieldProps) {

  const [showQR, setShowQR] = React.useState(false);

  const toggleQR = () => setShowQR(!showQR);
  const onScan = (data?: string) => {
    if (data !== null) {
      // TODO: we need to extract the label from the DigitalLink
      setEvent(updateFn(event, data));
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
            // defaultValue={}
            onChange={(e) => setEvent(updateFn(event, e.target.value))}
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
  const f = (s: string) => s.replace(/(?<=\w)[A-Z]/g, (x: string) => " " + x);

  return (
    <select defaultValue={props.value} onChange={(e) => props.setValue(e.target.value)}>
      {Object.keys(props.mapping).map((k: string) => (<option
        key={k}
        value={props.mapping[k]}
      >{f(k)}</option>))}
    </select>
  );
}
