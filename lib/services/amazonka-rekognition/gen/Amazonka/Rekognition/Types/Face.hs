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
-- Module      : Amazonka.Rekognition.Types.Face
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.Face where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.BoundingBox

-- | Describes the face properties such as the bounding box, face ID, image
-- ID of the input image, and external image ID that you assigned.
--
-- /See:/ 'newFace' smart constructor.
data Face = Face'
  { -- | Unique identifier that Amazon Rekognition assigns to the face.
    faceId :: Prelude.Maybe Prelude.Text,
    -- | Bounding box of the face.
    boundingBox :: Prelude.Maybe BoundingBox,
    -- | Identifier that you assign to all the faces in the input image.
    externalImageId :: Prelude.Maybe Prelude.Text,
    -- | Confidence level that the bounding box contains a face (and not a
    -- different object such as a tree).
    confidence :: Prelude.Maybe Prelude.Double,
    -- | Unique identifier that Amazon Rekognition assigns to the input image.
    imageId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Face' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceId', 'face_faceId' - Unique identifier that Amazon Rekognition assigns to the face.
--
-- 'boundingBox', 'face_boundingBox' - Bounding box of the face.
--
-- 'externalImageId', 'face_externalImageId' - Identifier that you assign to all the faces in the input image.
--
-- 'confidence', 'face_confidence' - Confidence level that the bounding box contains a face (and not a
-- different object such as a tree).
--
-- 'imageId', 'face_imageId' - Unique identifier that Amazon Rekognition assigns to the input image.
newFace ::
  Face
newFace =
  Face'
    { faceId = Prelude.Nothing,
      boundingBox = Prelude.Nothing,
      externalImageId = Prelude.Nothing,
      confidence = Prelude.Nothing,
      imageId = Prelude.Nothing
    }

-- | Unique identifier that Amazon Rekognition assigns to the face.
face_faceId :: Lens.Lens' Face (Prelude.Maybe Prelude.Text)
face_faceId = Lens.lens (\Face' {faceId} -> faceId) (\s@Face' {} a -> s {faceId = a} :: Face)

-- | Bounding box of the face.
face_boundingBox :: Lens.Lens' Face (Prelude.Maybe BoundingBox)
face_boundingBox = Lens.lens (\Face' {boundingBox} -> boundingBox) (\s@Face' {} a -> s {boundingBox = a} :: Face)

-- | Identifier that you assign to all the faces in the input image.
face_externalImageId :: Lens.Lens' Face (Prelude.Maybe Prelude.Text)
face_externalImageId = Lens.lens (\Face' {externalImageId} -> externalImageId) (\s@Face' {} a -> s {externalImageId = a} :: Face)

-- | Confidence level that the bounding box contains a face (and not a
-- different object such as a tree).
face_confidence :: Lens.Lens' Face (Prelude.Maybe Prelude.Double)
face_confidence = Lens.lens (\Face' {confidence} -> confidence) (\s@Face' {} a -> s {confidence = a} :: Face)

-- | Unique identifier that Amazon Rekognition assigns to the input image.
face_imageId :: Lens.Lens' Face (Prelude.Maybe Prelude.Text)
face_imageId = Lens.lens (\Face' {imageId} -> imageId) (\s@Face' {} a -> s {imageId = a} :: Face)

instance Core.FromJSON Face where
  parseJSON =
    Core.withObject
      "Face"
      ( \x ->
          Face'
            Prelude.<$> (x Core..:? "FaceId")
            Prelude.<*> (x Core..:? "BoundingBox")
            Prelude.<*> (x Core..:? "ExternalImageId")
            Prelude.<*> (x Core..:? "Confidence")
            Prelude.<*> (x Core..:? "ImageId")
      )

instance Prelude.Hashable Face where
  hashWithSalt salt' Face' {..} =
    salt' `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` externalImageId
      `Prelude.hashWithSalt` boundingBox
      `Prelude.hashWithSalt` faceId

instance Prelude.NFData Face where
  rnf Face' {..} =
    Prelude.rnf faceId
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf externalImageId
      `Prelude.seq` Prelude.rnf boundingBox
