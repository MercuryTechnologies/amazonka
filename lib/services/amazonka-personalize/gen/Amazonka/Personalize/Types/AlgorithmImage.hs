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
-- Module      : Amazonka.Personalize.Types.AlgorithmImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.AlgorithmImage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an algorithm image.
--
-- /See:/ 'newAlgorithmImage' smart constructor.
data AlgorithmImage = AlgorithmImage'
  { -- | The name of the algorithm image.
    name :: Prelude.Maybe Prelude.Text,
    -- | The URI of the Docker container for the algorithm image.
    dockerURI :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlgorithmImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'algorithmImage_name' - The name of the algorithm image.
--
-- 'dockerURI', 'algorithmImage_dockerURI' - The URI of the Docker container for the algorithm image.
newAlgorithmImage ::
  -- | 'dockerURI'
  Prelude.Text ->
  AlgorithmImage
newAlgorithmImage pDockerURI_ =
  AlgorithmImage'
    { name = Prelude.Nothing,
      dockerURI = pDockerURI_
    }

-- | The name of the algorithm image.
algorithmImage_name :: Lens.Lens' AlgorithmImage (Prelude.Maybe Prelude.Text)
algorithmImage_name = Lens.lens (\AlgorithmImage' {name} -> name) (\s@AlgorithmImage' {} a -> s {name = a} :: AlgorithmImage)

-- | The URI of the Docker container for the algorithm image.
algorithmImage_dockerURI :: Lens.Lens' AlgorithmImage Prelude.Text
algorithmImage_dockerURI = Lens.lens (\AlgorithmImage' {dockerURI} -> dockerURI) (\s@AlgorithmImage' {} a -> s {dockerURI = a} :: AlgorithmImage)

instance Core.FromJSON AlgorithmImage where
  parseJSON =
    Core.withObject
      "AlgorithmImage"
      ( \x ->
          AlgorithmImage'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..: "dockerURI")
      )

instance Prelude.Hashable AlgorithmImage where
  hashWithSalt salt' AlgorithmImage' {..} =
    salt' `Prelude.hashWithSalt` dockerURI
      `Prelude.hashWithSalt` name

instance Prelude.NFData AlgorithmImage where
  rnf AlgorithmImage' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf dockerURI
