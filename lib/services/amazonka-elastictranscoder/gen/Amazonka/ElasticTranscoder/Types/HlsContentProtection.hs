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
-- Module      : Amazonka.ElasticTranscoder.Types.HlsContentProtection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.HlsContentProtection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The HLS content protection settings, if any, that you want Elastic
-- Transcoder to apply to your output files.
--
-- /See:/ 'newHlsContentProtection' smart constructor.
data HlsContentProtection = HlsContentProtection'
  { -- | If Elastic Transcoder is generating your key for you, you must leave
    -- this field blank.
    --
    -- The MD5 digest of the key that you want Elastic Transcoder to use to
    -- encrypt your output file, and that you want Elastic Transcoder to use as
    -- a checksum to make sure your key was not corrupted in transit. The key
    -- MD5 must be base64-encoded, and it must be exactly 16 bytes before being
    -- base64- encoded.
    keyMd5 :: Prelude.Maybe Prelude.Text,
    -- | Specify whether you want Elastic Transcoder to write your HLS license
    -- key to an Amazon S3 bucket. If you choose @WithVariantPlaylists@,
    -- @LicenseAcquisitionUrl@ must be left blank and Elastic Transcoder writes
    -- your data key into the same bucket as the associated playlist.
    keyStoragePolicy :: Prelude.Maybe Prelude.Text,
    -- | If you want Elastic Transcoder to generate a key for you, leave this
    -- field blank.
    --
    -- If you choose to supply your own key, you must encrypt the key by using
    -- AWS KMS. The key must be base64-encoded, and it must be one of the
    -- following bit lengths before being base64-encoded:
    --
    -- @128@, @192@, or @256@.
    key :: Prelude.Maybe Prelude.Text,
    -- | The content protection method for your output. The only valid value is:
    -- @aes-128@.
    --
    -- This value is written into the method attribute of the @EXT-X-KEY@
    -- metadata tag in the output playlist.
    method :: Prelude.Maybe Prelude.Text,
    -- | If Elastic Transcoder is generating your key for you, you must leave
    -- this field blank.
    --
    -- The series of random bits created by a random bit generator, unique for
    -- every encryption operation, that you want Elastic Transcoder to use to
    -- encrypt your output files. The initialization vector must be
    -- base64-encoded, and it must be exactly 16 bytes before being
    -- base64-encoded.
    initializationVector :: Prelude.Maybe Prelude.Text,
    -- | The location of the license key required to decrypt your HLS playlist.
    -- The URL must be an absolute path, and is referenced in the URI attribute
    -- of the EXT-X-KEY metadata tag in the playlist file.
    licenseAcquisitionUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HlsContentProtection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyMd5', 'hlsContentProtection_keyMd5' - If Elastic Transcoder is generating your key for you, you must leave
-- this field blank.
--
-- The MD5 digest of the key that you want Elastic Transcoder to use to
-- encrypt your output file, and that you want Elastic Transcoder to use as
-- a checksum to make sure your key was not corrupted in transit. The key
-- MD5 must be base64-encoded, and it must be exactly 16 bytes before being
-- base64- encoded.
--
-- 'keyStoragePolicy', 'hlsContentProtection_keyStoragePolicy' - Specify whether you want Elastic Transcoder to write your HLS license
-- key to an Amazon S3 bucket. If you choose @WithVariantPlaylists@,
-- @LicenseAcquisitionUrl@ must be left blank and Elastic Transcoder writes
-- your data key into the same bucket as the associated playlist.
--
-- 'key', 'hlsContentProtection_key' - If you want Elastic Transcoder to generate a key for you, leave this
-- field blank.
--
-- If you choose to supply your own key, you must encrypt the key by using
-- AWS KMS. The key must be base64-encoded, and it must be one of the
-- following bit lengths before being base64-encoded:
--
-- @128@, @192@, or @256@.
--
-- 'method', 'hlsContentProtection_method' - The content protection method for your output. The only valid value is:
-- @aes-128@.
--
-- This value is written into the method attribute of the @EXT-X-KEY@
-- metadata tag in the output playlist.
--
-- 'initializationVector', 'hlsContentProtection_initializationVector' - If Elastic Transcoder is generating your key for you, you must leave
-- this field blank.
--
-- The series of random bits created by a random bit generator, unique for
-- every encryption operation, that you want Elastic Transcoder to use to
-- encrypt your output files. The initialization vector must be
-- base64-encoded, and it must be exactly 16 bytes before being
-- base64-encoded.
--
-- 'licenseAcquisitionUrl', 'hlsContentProtection_licenseAcquisitionUrl' - The location of the license key required to decrypt your HLS playlist.
-- The URL must be an absolute path, and is referenced in the URI attribute
-- of the EXT-X-KEY metadata tag in the playlist file.
newHlsContentProtection ::
  HlsContentProtection
newHlsContentProtection =
  HlsContentProtection'
    { keyMd5 = Prelude.Nothing,
      keyStoragePolicy = Prelude.Nothing,
      key = Prelude.Nothing,
      method = Prelude.Nothing,
      initializationVector = Prelude.Nothing,
      licenseAcquisitionUrl = Prelude.Nothing
    }

-- | If Elastic Transcoder is generating your key for you, you must leave
-- this field blank.
--
-- The MD5 digest of the key that you want Elastic Transcoder to use to
-- encrypt your output file, and that you want Elastic Transcoder to use as
-- a checksum to make sure your key was not corrupted in transit. The key
-- MD5 must be base64-encoded, and it must be exactly 16 bytes before being
-- base64- encoded.
hlsContentProtection_keyMd5 :: Lens.Lens' HlsContentProtection (Prelude.Maybe Prelude.Text)
hlsContentProtection_keyMd5 = Lens.lens (\HlsContentProtection' {keyMd5} -> keyMd5) (\s@HlsContentProtection' {} a -> s {keyMd5 = a} :: HlsContentProtection)

-- | Specify whether you want Elastic Transcoder to write your HLS license
-- key to an Amazon S3 bucket. If you choose @WithVariantPlaylists@,
-- @LicenseAcquisitionUrl@ must be left blank and Elastic Transcoder writes
-- your data key into the same bucket as the associated playlist.
hlsContentProtection_keyStoragePolicy :: Lens.Lens' HlsContentProtection (Prelude.Maybe Prelude.Text)
hlsContentProtection_keyStoragePolicy = Lens.lens (\HlsContentProtection' {keyStoragePolicy} -> keyStoragePolicy) (\s@HlsContentProtection' {} a -> s {keyStoragePolicy = a} :: HlsContentProtection)

-- | If you want Elastic Transcoder to generate a key for you, leave this
-- field blank.
--
-- If you choose to supply your own key, you must encrypt the key by using
-- AWS KMS. The key must be base64-encoded, and it must be one of the
-- following bit lengths before being base64-encoded:
--
-- @128@, @192@, or @256@.
hlsContentProtection_key :: Lens.Lens' HlsContentProtection (Prelude.Maybe Prelude.Text)
hlsContentProtection_key = Lens.lens (\HlsContentProtection' {key} -> key) (\s@HlsContentProtection' {} a -> s {key = a} :: HlsContentProtection)

-- | The content protection method for your output. The only valid value is:
-- @aes-128@.
--
-- This value is written into the method attribute of the @EXT-X-KEY@
-- metadata tag in the output playlist.
hlsContentProtection_method :: Lens.Lens' HlsContentProtection (Prelude.Maybe Prelude.Text)
hlsContentProtection_method = Lens.lens (\HlsContentProtection' {method} -> method) (\s@HlsContentProtection' {} a -> s {method = a} :: HlsContentProtection)

-- | If Elastic Transcoder is generating your key for you, you must leave
-- this field blank.
--
-- The series of random bits created by a random bit generator, unique for
-- every encryption operation, that you want Elastic Transcoder to use to
-- encrypt your output files. The initialization vector must be
-- base64-encoded, and it must be exactly 16 bytes before being
-- base64-encoded.
hlsContentProtection_initializationVector :: Lens.Lens' HlsContentProtection (Prelude.Maybe Prelude.Text)
hlsContentProtection_initializationVector = Lens.lens (\HlsContentProtection' {initializationVector} -> initializationVector) (\s@HlsContentProtection' {} a -> s {initializationVector = a} :: HlsContentProtection)

-- | The location of the license key required to decrypt your HLS playlist.
-- The URL must be an absolute path, and is referenced in the URI attribute
-- of the EXT-X-KEY metadata tag in the playlist file.
hlsContentProtection_licenseAcquisitionUrl :: Lens.Lens' HlsContentProtection (Prelude.Maybe Prelude.Text)
hlsContentProtection_licenseAcquisitionUrl = Lens.lens (\HlsContentProtection' {licenseAcquisitionUrl} -> licenseAcquisitionUrl) (\s@HlsContentProtection' {} a -> s {licenseAcquisitionUrl = a} :: HlsContentProtection)

instance Core.FromJSON HlsContentProtection where
  parseJSON =
    Core.withObject
      "HlsContentProtection"
      ( \x ->
          HlsContentProtection'
            Prelude.<$> (x Core..:? "KeyMd5")
            Prelude.<*> (x Core..:? "KeyStoragePolicy")
            Prelude.<*> (x Core..:? "Key")
            Prelude.<*> (x Core..:? "Method")
            Prelude.<*> (x Core..:? "InitializationVector")
            Prelude.<*> (x Core..:? "LicenseAcquisitionUrl")
      )

instance Prelude.Hashable HlsContentProtection where
  hashWithSalt salt' HlsContentProtection' {..} =
    salt' `Prelude.hashWithSalt` licenseAcquisitionUrl
      `Prelude.hashWithSalt` initializationVector
      `Prelude.hashWithSalt` method
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` keyStoragePolicy
      `Prelude.hashWithSalt` keyMd5

instance Prelude.NFData HlsContentProtection where
  rnf HlsContentProtection' {..} =
    Prelude.rnf keyMd5
      `Prelude.seq` Prelude.rnf licenseAcquisitionUrl
      `Prelude.seq` Prelude.rnf initializationVector
      `Prelude.seq` Prelude.rnf method
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf keyStoragePolicy

instance Core.ToJSON HlsContentProtection where
  toJSON HlsContentProtection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KeyMd5" Core..=) Prelude.<$> keyMd5,
            ("KeyStoragePolicy" Core..=)
              Prelude.<$> keyStoragePolicy,
            ("Key" Core..=) Prelude.<$> key,
            ("Method" Core..=) Prelude.<$> method,
            ("InitializationVector" Core..=)
              Prelude.<$> initializationVector,
            ("LicenseAcquisitionUrl" Core..=)
              Prelude.<$> licenseAcquisitionUrl
          ]
      )
