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
-- Module      : Amazonka.AuditManager.Types.ControlSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ControlSet where

import Amazonka.AuditManager.Types.Control
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A set of controls in Audit Manager.
--
-- /See:/ 'newControlSet' smart constructor.
data ControlSet = ControlSet'
  { -- | The list of controls within the control set.
    controls :: Prelude.Maybe (Prelude.NonEmpty Control),
    -- | The name of the control set.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the control set in the assessment. This is the control
    -- set name in a plain string format.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ControlSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'controls', 'controlSet_controls' - The list of controls within the control set.
--
-- 'name', 'controlSet_name' - The name of the control set.
--
-- 'id', 'controlSet_id' - The identifier of the control set in the assessment. This is the control
-- set name in a plain string format.
newControlSet ::
  ControlSet
newControlSet =
  ControlSet'
    { controls = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The list of controls within the control set.
controlSet_controls :: Lens.Lens' ControlSet (Prelude.Maybe (Prelude.NonEmpty Control))
controlSet_controls = Lens.lens (\ControlSet' {controls} -> controls) (\s@ControlSet' {} a -> s {controls = a} :: ControlSet) Prelude.. Lens.mapping Lens.coerced

-- | The name of the control set.
controlSet_name :: Lens.Lens' ControlSet (Prelude.Maybe Prelude.Text)
controlSet_name = Lens.lens (\ControlSet' {name} -> name) (\s@ControlSet' {} a -> s {name = a} :: ControlSet)

-- | The identifier of the control set in the assessment. This is the control
-- set name in a plain string format.
controlSet_id :: Lens.Lens' ControlSet (Prelude.Maybe Prelude.Text)
controlSet_id = Lens.lens (\ControlSet' {id} -> id) (\s@ControlSet' {} a -> s {id = a} :: ControlSet)

instance Core.FromJSON ControlSet where
  parseJSON =
    Core.withObject
      "ControlSet"
      ( \x ->
          ControlSet'
            Prelude.<$> (x Core..:? "controls")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "id")
      )

instance Prelude.Hashable ControlSet where
  hashWithSalt salt' ControlSet' {..} =
    salt' `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` controls

instance Prelude.NFData ControlSet where
  rnf ControlSet' {..} =
    Prelude.rnf controls `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
