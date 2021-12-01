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
-- Module      : Amazonka.Rekognition.Types.TestingData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.TestingData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.Asset

-- | The dataset used for testing. Optionally, if @AutoCreate@ is set, Amazon
-- Rekognition Custom Labels creates a testing dataset using an 80\/20
-- split of the training dataset.
--
-- /See:/ 'newTestingData' smart constructor.
data TestingData = TestingData'
  { -- | The assets used for testing.
    assets :: Prelude.Maybe [Asset],
    -- | If specified, Amazon Rekognition Custom Labels creates a testing dataset
    -- with an 80\/20 split of the training dataset.
    autoCreate :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestingData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assets', 'testingData_assets' - The assets used for testing.
--
-- 'autoCreate', 'testingData_autoCreate' - If specified, Amazon Rekognition Custom Labels creates a testing dataset
-- with an 80\/20 split of the training dataset.
newTestingData ::
  TestingData
newTestingData =
  TestingData'
    { assets = Prelude.Nothing,
      autoCreate = Prelude.Nothing
    }

-- | The assets used for testing.
testingData_assets :: Lens.Lens' TestingData (Prelude.Maybe [Asset])
testingData_assets = Lens.lens (\TestingData' {assets} -> assets) (\s@TestingData' {} a -> s {assets = a} :: TestingData) Prelude.. Lens.mapping Lens.coerced

-- | If specified, Amazon Rekognition Custom Labels creates a testing dataset
-- with an 80\/20 split of the training dataset.
testingData_autoCreate :: Lens.Lens' TestingData (Prelude.Maybe Prelude.Bool)
testingData_autoCreate = Lens.lens (\TestingData' {autoCreate} -> autoCreate) (\s@TestingData' {} a -> s {autoCreate = a} :: TestingData)

instance Core.FromJSON TestingData where
  parseJSON =
    Core.withObject
      "TestingData"
      ( \x ->
          TestingData'
            Prelude.<$> (x Core..:? "Assets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "AutoCreate")
      )

instance Prelude.Hashable TestingData where
  hashWithSalt salt' TestingData' {..} =
    salt' `Prelude.hashWithSalt` autoCreate
      `Prelude.hashWithSalt` assets

instance Prelude.NFData TestingData where
  rnf TestingData' {..} =
    Prelude.rnf assets
      `Prelude.seq` Prelude.rnf autoCreate

instance Core.ToJSON TestingData where
  toJSON TestingData' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Assets" Core..=) Prelude.<$> assets,
            ("AutoCreate" Core..=) Prelude.<$> autoCreate
          ]
      )
