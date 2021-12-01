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
-- Module      : Amazonka.MGN.Types.DataReplicationInitiationStep
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.DataReplicationInitiationStep where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MGN.Types.DataReplicationInitiationStepName
import Amazonka.MGN.Types.DataReplicationInitiationStepStatus
import qualified Amazonka.Prelude as Prelude

-- | Data replication intiation step.
--
-- /See:/ 'newDataReplicationInitiationStep' smart constructor.
data DataReplicationInitiationStep = DataReplicationInitiationStep'
  { -- | Request to query data initiation status.
    status :: Prelude.Maybe DataReplicationInitiationStepStatus,
    -- | Request to query data initiation step name.
    name :: Prelude.Maybe DataReplicationInitiationStepName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataReplicationInitiationStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dataReplicationInitiationStep_status' - Request to query data initiation status.
--
-- 'name', 'dataReplicationInitiationStep_name' - Request to query data initiation step name.
newDataReplicationInitiationStep ::
  DataReplicationInitiationStep
newDataReplicationInitiationStep =
  DataReplicationInitiationStep'
    { status =
        Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | Request to query data initiation status.
dataReplicationInitiationStep_status :: Lens.Lens' DataReplicationInitiationStep (Prelude.Maybe DataReplicationInitiationStepStatus)
dataReplicationInitiationStep_status = Lens.lens (\DataReplicationInitiationStep' {status} -> status) (\s@DataReplicationInitiationStep' {} a -> s {status = a} :: DataReplicationInitiationStep)

-- | Request to query data initiation step name.
dataReplicationInitiationStep_name :: Lens.Lens' DataReplicationInitiationStep (Prelude.Maybe DataReplicationInitiationStepName)
dataReplicationInitiationStep_name = Lens.lens (\DataReplicationInitiationStep' {name} -> name) (\s@DataReplicationInitiationStep' {} a -> s {name = a} :: DataReplicationInitiationStep)

instance Core.FromJSON DataReplicationInitiationStep where
  parseJSON =
    Core.withObject
      "DataReplicationInitiationStep"
      ( \x ->
          DataReplicationInitiationStep'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "name")
      )

instance
  Prelude.Hashable
    DataReplicationInitiationStep
  where
  hashWithSalt salt' DataReplicationInitiationStep' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData DataReplicationInitiationStep where
  rnf DataReplicationInitiationStep' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf name
