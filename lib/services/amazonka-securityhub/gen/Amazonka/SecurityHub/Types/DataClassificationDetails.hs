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
-- Module      : Amazonka.SecurityHub.Types.DataClassificationDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.DataClassificationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.ClassificationResult

-- | Provides details about sensitive data that was detected on a resource.
--
-- /See:/ 'newDataClassificationDetails' smart constructor.
data DataClassificationDetails = DataClassificationDetails'
  { -- | The path to the folder or file that contains the sensitive data.
    detailedResultsLocation :: Prelude.Maybe Prelude.Text,
    -- | The details about the sensitive data that was detected on the resource.
    result :: Prelude.Maybe ClassificationResult
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataClassificationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detailedResultsLocation', 'dataClassificationDetails_detailedResultsLocation' - The path to the folder or file that contains the sensitive data.
--
-- 'result', 'dataClassificationDetails_result' - The details about the sensitive data that was detected on the resource.
newDataClassificationDetails ::
  DataClassificationDetails
newDataClassificationDetails =
  DataClassificationDetails'
    { detailedResultsLocation =
        Prelude.Nothing,
      result = Prelude.Nothing
    }

-- | The path to the folder or file that contains the sensitive data.
dataClassificationDetails_detailedResultsLocation :: Lens.Lens' DataClassificationDetails (Prelude.Maybe Prelude.Text)
dataClassificationDetails_detailedResultsLocation = Lens.lens (\DataClassificationDetails' {detailedResultsLocation} -> detailedResultsLocation) (\s@DataClassificationDetails' {} a -> s {detailedResultsLocation = a} :: DataClassificationDetails)

-- | The details about the sensitive data that was detected on the resource.
dataClassificationDetails_result :: Lens.Lens' DataClassificationDetails (Prelude.Maybe ClassificationResult)
dataClassificationDetails_result = Lens.lens (\DataClassificationDetails' {result} -> result) (\s@DataClassificationDetails' {} a -> s {result = a} :: DataClassificationDetails)

instance Core.FromJSON DataClassificationDetails where
  parseJSON =
    Core.withObject
      "DataClassificationDetails"
      ( \x ->
          DataClassificationDetails'
            Prelude.<$> (x Core..:? "DetailedResultsLocation")
            Prelude.<*> (x Core..:? "Result")
      )

instance Prelude.Hashable DataClassificationDetails where
  hashWithSalt salt' DataClassificationDetails' {..} =
    salt' `Prelude.hashWithSalt` result
      `Prelude.hashWithSalt` detailedResultsLocation

instance Prelude.NFData DataClassificationDetails where
  rnf DataClassificationDetails' {..} =
    Prelude.rnf detailedResultsLocation
      `Prelude.seq` Prelude.rnf result

instance Core.ToJSON DataClassificationDetails where
  toJSON DataClassificationDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DetailedResultsLocation" Core..=)
              Prelude.<$> detailedResultsLocation,
            ("Result" Core..=) Prelude.<$> result
          ]
      )
