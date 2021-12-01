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
-- Module      : Amazonka.Lambda.Types.LayerVersionContentOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.LayerVersionContentOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about a version of an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html Lambda layer>.
--
-- /See:/ 'newLayerVersionContentOutput' smart constructor.
data LayerVersionContentOutput = LayerVersionContentOutput'
  { -- | The Amazon Resource Name (ARN) for a signing profile version.
    signingProfileVersionArn :: Prelude.Maybe Prelude.Text,
    -- | A link to the layer archive in Amazon S3 that is valid for 10 minutes.
    location :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a signing job.
    signingJobArn :: Prelude.Maybe Prelude.Text,
    -- | The size of the layer archive in bytes.
    codeSize :: Prelude.Maybe Prelude.Integer,
    -- | The SHA-256 hash of the layer archive.
    codeSha256 :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LayerVersionContentOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signingProfileVersionArn', 'layerVersionContentOutput_signingProfileVersionArn' - The Amazon Resource Name (ARN) for a signing profile version.
--
-- 'location', 'layerVersionContentOutput_location' - A link to the layer archive in Amazon S3 that is valid for 10 minutes.
--
-- 'signingJobArn', 'layerVersionContentOutput_signingJobArn' - The Amazon Resource Name (ARN) of a signing job.
--
-- 'codeSize', 'layerVersionContentOutput_codeSize' - The size of the layer archive in bytes.
--
-- 'codeSha256', 'layerVersionContentOutput_codeSha256' - The SHA-256 hash of the layer archive.
newLayerVersionContentOutput ::
  LayerVersionContentOutput
newLayerVersionContentOutput =
  LayerVersionContentOutput'
    { signingProfileVersionArn =
        Prelude.Nothing,
      location = Prelude.Nothing,
      signingJobArn = Prelude.Nothing,
      codeSize = Prelude.Nothing,
      codeSha256 = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for a signing profile version.
layerVersionContentOutput_signingProfileVersionArn :: Lens.Lens' LayerVersionContentOutput (Prelude.Maybe Prelude.Text)
layerVersionContentOutput_signingProfileVersionArn = Lens.lens (\LayerVersionContentOutput' {signingProfileVersionArn} -> signingProfileVersionArn) (\s@LayerVersionContentOutput' {} a -> s {signingProfileVersionArn = a} :: LayerVersionContentOutput)

-- | A link to the layer archive in Amazon S3 that is valid for 10 minutes.
layerVersionContentOutput_location :: Lens.Lens' LayerVersionContentOutput (Prelude.Maybe Prelude.Text)
layerVersionContentOutput_location = Lens.lens (\LayerVersionContentOutput' {location} -> location) (\s@LayerVersionContentOutput' {} a -> s {location = a} :: LayerVersionContentOutput)

-- | The Amazon Resource Name (ARN) of a signing job.
layerVersionContentOutput_signingJobArn :: Lens.Lens' LayerVersionContentOutput (Prelude.Maybe Prelude.Text)
layerVersionContentOutput_signingJobArn = Lens.lens (\LayerVersionContentOutput' {signingJobArn} -> signingJobArn) (\s@LayerVersionContentOutput' {} a -> s {signingJobArn = a} :: LayerVersionContentOutput)

-- | The size of the layer archive in bytes.
layerVersionContentOutput_codeSize :: Lens.Lens' LayerVersionContentOutput (Prelude.Maybe Prelude.Integer)
layerVersionContentOutput_codeSize = Lens.lens (\LayerVersionContentOutput' {codeSize} -> codeSize) (\s@LayerVersionContentOutput' {} a -> s {codeSize = a} :: LayerVersionContentOutput)

-- | The SHA-256 hash of the layer archive.
layerVersionContentOutput_codeSha256 :: Lens.Lens' LayerVersionContentOutput (Prelude.Maybe Prelude.Text)
layerVersionContentOutput_codeSha256 = Lens.lens (\LayerVersionContentOutput' {codeSha256} -> codeSha256) (\s@LayerVersionContentOutput' {} a -> s {codeSha256 = a} :: LayerVersionContentOutput)

instance Core.FromJSON LayerVersionContentOutput where
  parseJSON =
    Core.withObject
      "LayerVersionContentOutput"
      ( \x ->
          LayerVersionContentOutput'
            Prelude.<$> (x Core..:? "SigningProfileVersionArn")
            Prelude.<*> (x Core..:? "Location")
            Prelude.<*> (x Core..:? "SigningJobArn")
            Prelude.<*> (x Core..:? "CodeSize")
            Prelude.<*> (x Core..:? "CodeSha256")
      )

instance Prelude.Hashable LayerVersionContentOutput where
  hashWithSalt salt' LayerVersionContentOutput' {..} =
    salt' `Prelude.hashWithSalt` codeSha256
      `Prelude.hashWithSalt` codeSize
      `Prelude.hashWithSalt` signingJobArn
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` signingProfileVersionArn

instance Prelude.NFData LayerVersionContentOutput where
  rnf LayerVersionContentOutput' {..} =
    Prelude.rnf signingProfileVersionArn
      `Prelude.seq` Prelude.rnf codeSha256
      `Prelude.seq` Prelude.rnf codeSize
      `Prelude.seq` Prelude.rnf signingJobArn
      `Prelude.seq` Prelude.rnf location
