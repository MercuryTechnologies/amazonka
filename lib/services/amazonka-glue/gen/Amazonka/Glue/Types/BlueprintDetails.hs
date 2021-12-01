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
-- Module      : Amazonka.Glue.Types.BlueprintDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.BlueprintDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of a blueprint.
--
-- /See:/ 'newBlueprintDetails' smart constructor.
data BlueprintDetails = BlueprintDetails'
  { -- | The run ID for this blueprint.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The name of the blueprint.
    blueprintName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BlueprintDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'blueprintDetails_runId' - The run ID for this blueprint.
--
-- 'blueprintName', 'blueprintDetails_blueprintName' - The name of the blueprint.
newBlueprintDetails ::
  BlueprintDetails
newBlueprintDetails =
  BlueprintDetails'
    { runId = Prelude.Nothing,
      blueprintName = Prelude.Nothing
    }

-- | The run ID for this blueprint.
blueprintDetails_runId :: Lens.Lens' BlueprintDetails (Prelude.Maybe Prelude.Text)
blueprintDetails_runId = Lens.lens (\BlueprintDetails' {runId} -> runId) (\s@BlueprintDetails' {} a -> s {runId = a} :: BlueprintDetails)

-- | The name of the blueprint.
blueprintDetails_blueprintName :: Lens.Lens' BlueprintDetails (Prelude.Maybe Prelude.Text)
blueprintDetails_blueprintName = Lens.lens (\BlueprintDetails' {blueprintName} -> blueprintName) (\s@BlueprintDetails' {} a -> s {blueprintName = a} :: BlueprintDetails)

instance Core.FromJSON BlueprintDetails where
  parseJSON =
    Core.withObject
      "BlueprintDetails"
      ( \x ->
          BlueprintDetails'
            Prelude.<$> (x Core..:? "RunId")
            Prelude.<*> (x Core..:? "BlueprintName")
      )

instance Prelude.Hashable BlueprintDetails where
  hashWithSalt salt' BlueprintDetails' {..} =
    salt' `Prelude.hashWithSalt` blueprintName
      `Prelude.hashWithSalt` runId

instance Prelude.NFData BlueprintDetails where
  rnf BlueprintDetails' {..} =
    Prelude.rnf runId
      `Prelude.seq` Prelude.rnf blueprintName
