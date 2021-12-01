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
-- Module      : Amazonka.GroundStation.Types.Source
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.Source where

import qualified Amazonka.Core as Core
import Amazonka.GroundStation.Types.ConfigCapabilityType
import Amazonka.GroundStation.Types.ConfigDetails
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Dataflow details for the source side.
--
-- /See:/ 'newSource' smart constructor.
data Source = Source'
  { -- | Region of a dataflow source.
    dataflowSourceRegion :: Prelude.Maybe Prelude.Text,
    -- | UUID of a @Config@.
    configId :: Prelude.Maybe Prelude.Text,
    -- | Type of a @Config@.
    configType :: Prelude.Maybe ConfigCapabilityType,
    -- | Additional details for a @Config@, if type is dataflow endpoint or
    -- antenna demod decode.
    configDetails :: Prelude.Maybe ConfigDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Source' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataflowSourceRegion', 'source_dataflowSourceRegion' - Region of a dataflow source.
--
-- 'configId', 'source_configId' - UUID of a @Config@.
--
-- 'configType', 'source_configType' - Type of a @Config@.
--
-- 'configDetails', 'source_configDetails' - Additional details for a @Config@, if type is dataflow endpoint or
-- antenna demod decode.
newSource ::
  Source
newSource =
  Source'
    { dataflowSourceRegion = Prelude.Nothing,
      configId = Prelude.Nothing,
      configType = Prelude.Nothing,
      configDetails = Prelude.Nothing
    }

-- | Region of a dataflow source.
source_dataflowSourceRegion :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_dataflowSourceRegion = Lens.lens (\Source' {dataflowSourceRegion} -> dataflowSourceRegion) (\s@Source' {} a -> s {dataflowSourceRegion = a} :: Source)

-- | UUID of a @Config@.
source_configId :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_configId = Lens.lens (\Source' {configId} -> configId) (\s@Source' {} a -> s {configId = a} :: Source)

-- | Type of a @Config@.
source_configType :: Lens.Lens' Source (Prelude.Maybe ConfigCapabilityType)
source_configType = Lens.lens (\Source' {configType} -> configType) (\s@Source' {} a -> s {configType = a} :: Source)

-- | Additional details for a @Config@, if type is dataflow endpoint or
-- antenna demod decode.
source_configDetails :: Lens.Lens' Source (Prelude.Maybe ConfigDetails)
source_configDetails = Lens.lens (\Source' {configDetails} -> configDetails) (\s@Source' {} a -> s {configDetails = a} :: Source)

instance Core.FromJSON Source where
  parseJSON =
    Core.withObject
      "Source"
      ( \x ->
          Source'
            Prelude.<$> (x Core..:? "dataflowSourceRegion")
            Prelude.<*> (x Core..:? "configId")
            Prelude.<*> (x Core..:? "configType")
            Prelude.<*> (x Core..:? "configDetails")
      )

instance Prelude.Hashable Source where
  hashWithSalt salt' Source' {..} =
    salt' `Prelude.hashWithSalt` configDetails
      `Prelude.hashWithSalt` configType
      `Prelude.hashWithSalt` configId
      `Prelude.hashWithSalt` dataflowSourceRegion

instance Prelude.NFData Source where
  rnf Source' {..} =
    Prelude.rnf dataflowSourceRegion
      `Prelude.seq` Prelude.rnf configDetails
      `Prelude.seq` Prelude.rnf configType
      `Prelude.seq` Prelude.rnf configId
