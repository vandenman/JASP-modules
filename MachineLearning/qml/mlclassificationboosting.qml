//
// Copyright (C) 2013-2019 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Theme 1.0

// All Analysis forms must be built with the From QML item
Form
{
    usesJaspResults: true

    VariablesForm {
        AssignedVariablesList {
            name: "target"
            title: qsTr("Target")
            singleItem: true
            allowedColumns: ["nominal", "ordinal"]
        }
        AssignedVariablesList {
                    name: "predictors"
                    title: qsTr("Predictors")
                    singleItem: false
                    allowedColumns: ["nominal", "scale", "ordinal"]
                }
        AssignedVariablesList {
                    name: "indicator"
                    title: qsTr("Apply indicator (optional)")
                    singleItem: true
                    allowedColumns: ["nominal"]
                }
    }

    GroupBox {
        title: qsTr("Tables")

        CheckBox { name: "classBoostConfTable";	    text: qsTr("Confusion Table")         ; checked: true   }
        CheckBox { name: "classBoostRelInfTable";	text: qsTr("Relative Influence Table")                  }
    }

    ExpanderButton
    {
        title: qsTr("Model Specifications")

        GridLayout
        {

            RadioButtonGroup {
                title: qsTr("Number of Trees for Training")
                name: "noOfTrees"
                RadioButton { text: qsTr("Auto")        ; name: "auto"  ; checked: true}
                RowLayout {
                    RadioButton { text: qsTr("Manual")  ; name: "manual"; id: numberOfTrees }
                    TextField {
                        inputType: "integer"
                        name: "numberOfTrees"
                        validator: IntValidator {bottom: 1; top: 999999}
                        fieldWidth: 75
                        enabled: numberOfTrees.checked
                    }
                }
            }

            RadioButtonGroup {
                title: qsTr("Shrinkage")
                name: "shrinkage"
                RadioButton { text: qsTr("Auto")        ; name: "auto"  ; checked: true}
                RowLayout {
                    RadioButton { text: qsTr("Manual")  ; name: "manual"; id: shrinkage }
                    TextField
                    {
                        property double defaultValue:		0.1
                        property bool	validation:			true

                                        inputType:			"number"
                                        name:               "shrinkage.parameter"
                                        validator:			DoubleValidator { bottom: 0; top: 1 ; decimals: 5 }
                                        value:				Number.parseFloat(defaultValue);
                                        enabled:            shrinkage.checked
                                        fieldWidth:         75
                    }
                }
            }

            RadioButtonGroup {
                title: qsTr("Interaction Depth")
                name: "int.depth"
                RadioButton { text: qsTr("Auto")        ; name: "auto"  ; checked: true}
                RowLayout {
                    RadioButton { text: qsTr("Manual")  ; name: "manual"; id: interaction }
                    TextField {
                        inputType: "integer"
                        name: "int.depth.parameter"
                        validator: IntValidator {bottom: 1; top: 999}
                        enabled: interaction.checked
                    }
                }
            }

            RadioButtonGroup {
                title: qsTr("Min. No. Observations in Node")
                name: "nNode"
                RadioButton { text: qsTr("Auto")        ; name: "auto"  ; checked: true}
                RowLayout {
                    RadioButton { text: qsTr("Manual")  ; name: "manual"; id: nNode }
                    TextField {
                        inputType: "integer"
                        name: "nNodeSpec"
                        validator: IntValidator {bottom: 1; top: 999}
                        enabled: nNode.checked
                    }
                }
            }

            RadioButtonGroup {
                title: qsTr("CV Folds")
                name: "cvFolds"
                RadioButton { text: qsTr("Auto")        ; name: "auto"  ; checked: true}
                RowLayout {
                    RadioButton { text: qsTr("Manual")  ; name: "manual"; id: cvFolds }
                    TextField {
                        inputType: "integer"
                        name: "cvFoldsSpec"
                        validator: IntValidator {bottom: 1; top: 99}
                        enabled: cvFolds.checked
                    }
                }
            }

            RadioButtonGroup
            {
                name: "dataTrain"
                RadioButton { value: "auto";	text: qsTr("Auto"); checked: true           }
                RadioButton { value: "manual";	text: qsTr("Manual"); id: dataTrain	}
                PercentField
                {
                    name: "percentageDataTraining"
                    text: qsTr("Data used for training:")
                    defaultValue: 80
                    enabled: dataTrain.checked
                    indent: true
                }
            }

            RadioButtonGroup
            {
                name: "bag.fraction"
                RadioButton { value: "auto";	text: qsTr("Auto"); checked: true           }
                RadioButton { value: "manual";	text: qsTr("Manual"); id: bagfraction	}
                PercentField
                {
                    name: "bag.fraction.spec"
                    text: qsTr("Training data used per tree:")
                    defaultValue: 50
                    enabled: bagfraction.checked
                    indent: true
                }
            }

        }
    }

    ExpanderButton
    {
        title: qsTr("Plots")

        Group
        {
            CheckBox { name: "plotRelInf";       text: qsTr("Relative influence plot")   }
            CheckBox { name: "plotDeviance";     text: qsTr("Deviance plot")             }
            CheckBox { name: "plotOOBChangeDev"; text: qsTr("OOB improvement plot")      }
        }
    }

    ExpanderButton
    {
        title: qsTr("Advanced")

        GridLayout{

            GroupBox
            {

            title: qsTr("Set seed")

            RadioButtonGroup
            {
                name: "seedBox"
                RadioButton { value: "auto";	text: qsTr("Auto"); checked: true           }
                RadioButton { value: "manual";	text: qsTr("Manual"); id: seedBox           }
                DoubleField
                {
                    name: "seed"
                    text: qsTr("Seed:")
                    doubleValidator { bottom: -99999999; top: 99999999; decimals: 2 }
                    defaultValue: 1
                    fieldWidth: 50
                    enabled: seedBox.checked
                    indent: true
                }
            }
            }

            GroupBox
            {

            title: qsTr("Missing values")

            RadioButtonGroup
            {
                name: "missingValues"
                RadioButton { value: "na.omit";	text: qsTr("Omit NA rows"); checked: true                 }
                RadioButton { value: "roughfix";	text: qsTr("Apply roughfix to NAs"); id: naAction          }
            }
            }

        }
    }
}
