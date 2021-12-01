{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTEvents.Types.SetVariableAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.SetVariableAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the variable and its new value.
--
-- /See:/ 'newSetVariableAction' smart constructor.
data SetVariableAction = SetVariableAction'
  { -- | The name of the variable.
    variableName :: Prelude.Text,
    -- | The new value of the variable.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetVariableAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'variableName', 'setVariableAction_variableName' - The name of the variable.
--
-- 'value', 'setVariableAction_value' - The new value of the variable.
newSetVariableAction ::
  -- | 'variableName'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  SetVariableAction
newSetVariableAction pVariableName_ pValue_ =
  SetVariableAction'
    { variableName = pVariableName_,
      value = pValue_
    }

-- | The name of the variable.
setVariableAction_variableName :: Lens.Lens' SetVariableAction Prelude.Text
setVariableAction_variableName = Lens.lens (\SetVariableAction' {variableName} -> variableName) (\s@SetVariableAction' {} a -> s {variableName = a} :: SetVariableAction)

-- | The new value of the variable.
setVariableAction_value :: Lens.Lens' SetVariableAction Prelude.Text
setVariableAction_value = Lens.lens (\SetVariableAction' {value} -> value) (\s@SetVariableAction' {} a -> s {value = a} :: SetVariableAction)

instance Core.FromJSON SetVariableAction where
  parseJSON =
    Core.withObject
      "SetVariableAction"
      ( \x ->
          SetVariableAction'
            Prelude.<$> (x Core..: "variableName")
            Prelude.<*> (x Core..: "value")
      )

instance Prelude.Hashable SetVariableAction where
  hashWithSalt salt' SetVariableAction' {..} =
    salt' `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` variableName

instance Prelude.NFData SetVariableAction where
  rnf SetVariableAction' {..} =
    Prelude.rnf variableName
      `Prelude.seq` Prelude.rnf value

instance Core.ToJSON SetVariableAction where
  toJSON SetVariableAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("variableName" Core..= variableName),
            Prelude.Just ("value" Core..= value)
          ]
      )
