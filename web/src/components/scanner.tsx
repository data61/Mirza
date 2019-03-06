import * as React from "react";
import * as QrReader from 'react-qr-reader';

export class MyScanner extends React.Component {
  state = {
    result: 'No result'
  }
 
  handleScan = (data: any) => {
    // console.log(data);
    if (data) {
      console.log(data);
      this.setState({
        result: data
      })
    }
  }
  handleError = function (err: any) {
    console.error(err);
  }
  render() {
    return (
      <div>
        <QrReader
          delay={ 300 }
          onError={this.handleError}
          onScan={this.handleScan}
          style={{ width: '30%' }}
        />
        <p> {this.state.result} </p>
      </div>
    )
  }
}
