{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Schemas.UpdateSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the schema definition
--
-- Inactive schemas will be deleted after two years.
module Amazonka.Schemas.UpdateSchema
  ( -- * Creating a Request
    UpdateSchema (..),
    newUpdateSchema,

    -- * Request Lenses
    updateSchema_content,
    updateSchema_clientTokenId,
    updateSchema_type,
    updateSchema_description,
    updateSchema_registryName,
    updateSchema_schemaName,

    -- * Destructuring the Response
    UpdateSchemaResponse (..),
    newUpdateSchemaResponse,

    -- * Response Lenses
    updateSchemaResponse_schemaVersion,
    updateSchemaResponse_schemaName,
    updateSchemaResponse_schemaArn,
    updateSchemaResponse_type,
    updateSchemaResponse_lastModified,
    updateSchemaResponse_description,
    updateSchemaResponse_versionCreatedDate,
    updateSchemaResponse_tags,
    updateSchemaResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | /See:/ 'newUpdateSchema' smart constructor.
data UpdateSchema = UpdateSchema'
  { -- | The source of the schema definition.
    content :: Prelude.Maybe Prelude.Text,
    -- | The ID of the client token.
    clientTokenId :: Prelude.Maybe Prelude.Text,
    -- | The schema type for the events schema.
    type' :: Prelude.Maybe Type,
    -- | The description of the schema.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the registry.
    registryName :: Prelude.Text,
    -- | The name of the schema.
    schemaName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'updateSchema_content' - The source of the schema definition.
--
-- 'clientTokenId', 'updateSchema_clientTokenId' - The ID of the client token.
--
-- 'type'', 'updateSchema_type' - The schema type for the events schema.
--
-- 'description', 'updateSchema_description' - The description of the schema.
--
-- 'registryName', 'updateSchema_registryName' - The name of the registry.
--
-- 'schemaName', 'updateSchema_schemaName' - The name of the schema.
newUpdateSchema ::
  -- | 'registryName'
  Prelude.Text ->
  -- | 'schemaName'
  Prelude.Text ->
  UpdateSchema
newUpdateSchema pRegistryName_ pSchemaName_ =
  UpdateSchema'
    { content = Prelude.Nothing,
      clientTokenId = Prelude.Nothing,
      type' = Prelude.Nothing,
      description = Prelude.Nothing,
      registryName = pRegistryName_,
      schemaName = pSchemaName_
    }

-- | The source of the schema definition.
updateSchema_content :: Lens.Lens' UpdateSchema (Prelude.Maybe Prelude.Text)
updateSchema_content = Lens.lens (\UpdateSchema' {content} -> content) (\s@UpdateSchema' {} a -> s {content = a} :: UpdateSchema)

-- | The ID of the client token.
updateSchema_clientTokenId :: Lens.Lens' UpdateSchema (Prelude.Maybe Prelude.Text)
updateSchema_clientTokenId = Lens.lens (\UpdateSchema' {clientTokenId} -> clientTokenId) (\s@UpdateSchema' {} a -> s {clientTokenId = a} :: UpdateSchema)

-- | The schema type for the events schema.
updateSchema_type :: Lens.Lens' UpdateSchema (Prelude.Maybe Type)
updateSchema_type = Lens.lens (\UpdateSchema' {type'} -> type') (\s@UpdateSchema' {} a -> s {type' = a} :: UpdateSchema)

-- | The description of the schema.
updateSchema_description :: Lens.Lens' UpdateSchema (Prelude.Maybe Prelude.Text)
updateSchema_description = Lens.lens (\UpdateSchema' {description} -> description) (\s@UpdateSchema' {} a -> s {description = a} :: UpdateSchema)

-- | The name of the registry.
updateSchema_registryName :: Lens.Lens' UpdateSchema Prelude.Text
updateSchema_registryName = Lens.lens (\UpdateSchema' {registryName} -> registryName) (\s@UpdateSchema' {} a -> s {registryName = a} :: UpdateSchema)

-- | The name of the schema.
updateSchema_schemaName :: Lens.Lens' UpdateSchema Prelude.Text
updateSchema_schemaName = Lens.lens (\UpdateSchema' {schemaName} -> schemaName) (\s@UpdateSchema' {} a -> s {schemaName = a} :: UpdateSchema)

instance Core.AWSRequest UpdateSchema where
  type AWSResponse UpdateSchema = UpdateSchemaResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSchemaResponse'
            Prelude.<$> (x Core..?> "SchemaVersion")
            Prelude.<*> (x Core..?> "SchemaName")
            Prelude.<*> (x Core..?> "SchemaArn")
            Prelude.<*> (x Core..?> "Type")
            Prelude.<*> (x Core..?> "LastModified")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "VersionCreatedDate")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSchema where
  hashWithSalt salt' UpdateSchema' {..} =
    salt' `Prelude.hashWithSalt` schemaName
      `Prelude.hashWithSalt` registryName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` clientTokenId
      `Prelude.hashWithSalt` content

instance Prelude.NFData UpdateSchema where
  rnf UpdateSchema' {..} =
    Prelude.rnf content
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf registryName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf clientTokenId

instance Core.ToHeaders UpdateSchema where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSchema where
  toJSON UpdateSchema' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Content" Core..=) Prelude.<$> content,
            ("ClientTokenId" Core..=) Prelude.<$> clientTokenId,
            ("Type" Core..=) Prelude.<$> type',
            ("Description" Core..=) Prelude.<$> description
          ]
      )

instance Core.ToPath UpdateSchema where
  toPath UpdateSchema' {..} =
    Prelude.mconcat
      [ "/v1/registries/name/",
        Core.toBS registryName,
        "/schemas/name/",
        Core.toBS schemaName
      ]

instance Core.ToQuery UpdateSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSchemaResponse' smart constructor.
data UpdateSchemaResponse = UpdateSchemaResponse'
  { -- | The version number of the schema
    schemaVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The type of the schema.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The date and time that schema was modified.
    lastModified :: Prelude.Maybe Core.POSIX,
    -- | The description of the schema.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date the schema version was created.
    versionCreatedDate :: Prelude.Maybe Core.POSIX,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaVersion', 'updateSchemaResponse_schemaVersion' - The version number of the schema
--
-- 'schemaName', 'updateSchemaResponse_schemaName' - The name of the schema.
--
-- 'schemaArn', 'updateSchemaResponse_schemaArn' - The ARN of the schema.
--
-- 'type'', 'updateSchemaResponse_type' - The type of the schema.
--
-- 'lastModified', 'updateSchemaResponse_lastModified' - The date and time that schema was modified.
--
-- 'description', 'updateSchemaResponse_description' - The description of the schema.
--
-- 'versionCreatedDate', 'updateSchemaResponse_versionCreatedDate' - The date the schema version was created.
--
-- 'tags', 'updateSchemaResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'updateSchemaResponse_httpStatus' - The response's http status code.
newUpdateSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSchemaResponse
newUpdateSchemaResponse pHttpStatus_ =
  UpdateSchemaResponse'
    { schemaVersion =
        Prelude.Nothing,
      schemaName = Prelude.Nothing,
      schemaArn = Prelude.Nothing,
      type' = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      description = Prelude.Nothing,
      versionCreatedDate = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version number of the schema
updateSchemaResponse_schemaVersion :: Lens.Lens' UpdateSchemaResponse (Prelude.Maybe Prelude.Text)
updateSchemaResponse_schemaVersion = Lens.lens (\UpdateSchemaResponse' {schemaVersion} -> schemaVersion) (\s@UpdateSchemaResponse' {} a -> s {schemaVersion = a} :: UpdateSchemaResponse)

-- | The name of the schema.
updateSchemaResponse_schemaName :: Lens.Lens' UpdateSchemaResponse (Prelude.Maybe Prelude.Text)
updateSchemaResponse_schemaName = Lens.lens (\UpdateSchemaResponse' {schemaName} -> schemaName) (\s@UpdateSchemaResponse' {} a -> s {schemaName = a} :: UpdateSchemaResponse)

-- | The ARN of the schema.
updateSchemaResponse_schemaArn :: Lens.Lens' UpdateSchemaResponse (Prelude.Maybe Prelude.Text)
updateSchemaResponse_schemaArn = Lens.lens (\UpdateSchemaResponse' {schemaArn} -> schemaArn) (\s@UpdateSchemaResponse' {} a -> s {schemaArn = a} :: UpdateSchemaResponse)

-- | The type of the schema.
updateSchemaResponse_type :: Lens.Lens' UpdateSchemaResponse (Prelude.Maybe Prelude.Text)
updateSchemaResponse_type = Lens.lens (\UpdateSchemaResponse' {type'} -> type') (\s@UpdateSchemaResponse' {} a -> s {type' = a} :: UpdateSchemaResponse)

-- | The date and time that schema was modified.
updateSchemaResponse_lastModified :: Lens.Lens' UpdateSchemaResponse (Prelude.Maybe Prelude.UTCTime)
updateSchemaResponse_lastModified = Lens.lens (\UpdateSchemaResponse' {lastModified} -> lastModified) (\s@UpdateSchemaResponse' {} a -> s {lastModified = a} :: UpdateSchemaResponse) Prelude.. Lens.mapping Core._Time

-- | The description of the schema.
updateSchemaResponse_description :: Lens.Lens' UpdateSchemaResponse (Prelude.Maybe Prelude.Text)
updateSchemaResponse_description = Lens.lens (\UpdateSchemaResponse' {description} -> description) (\s@UpdateSchemaResponse' {} a -> s {description = a} :: UpdateSchemaResponse)

-- | The date the schema version was created.
updateSchemaResponse_versionCreatedDate :: Lens.Lens' UpdateSchemaResponse (Prelude.Maybe Prelude.UTCTime)
updateSchemaResponse_versionCreatedDate = Lens.lens (\UpdateSchemaResponse' {versionCreatedDate} -> versionCreatedDate) (\s@UpdateSchemaResponse' {} a -> s {versionCreatedDate = a} :: UpdateSchemaResponse) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
updateSchemaResponse_tags :: Lens.Lens' UpdateSchemaResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateSchemaResponse_tags = Lens.lens (\UpdateSchemaResponse' {tags} -> tags) (\s@UpdateSchemaResponse' {} a -> s {tags = a} :: UpdateSchemaResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateSchemaResponse_httpStatus :: Lens.Lens' UpdateSchemaResponse Prelude.Int
updateSchemaResponse_httpStatus = Lens.lens (\UpdateSchemaResponse' {httpStatus} -> httpStatus) (\s@UpdateSchemaResponse' {} a -> s {httpStatus = a} :: UpdateSchemaResponse)

instance Prelude.NFData UpdateSchemaResponse where
  rnf UpdateSchemaResponse' {..} =
    Prelude.rnf schemaVersion
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf versionCreatedDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf schemaName
