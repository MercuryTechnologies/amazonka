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
-- Module      : Amazonka.DynamoDB.Types.TableDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.TableDescription where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDB.Types.ArchivalSummary
import Amazonka.DynamoDB.Types.AttributeDefinition
import Amazonka.DynamoDB.Types.BillingModeSummary
import Amazonka.DynamoDB.Types.GlobalSecondaryIndexDescription
import Amazonka.DynamoDB.Types.KeySchemaElement
import Amazonka.DynamoDB.Types.LocalSecondaryIndexDescription
import Amazonka.DynamoDB.Types.ProvisionedThroughputDescription
import Amazonka.DynamoDB.Types.ReplicaDescription
import Amazonka.DynamoDB.Types.RestoreSummary
import Amazonka.DynamoDB.Types.SSEDescription
import Amazonka.DynamoDB.Types.StreamSpecification
import Amazonka.DynamoDB.Types.TableStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of a table.
--
-- /See:/ 'newTableDescription' smart constructor.
data TableDescription = TableDescription'
  { -- | Contains details for the restore.
    restoreSummary :: Prelude.Maybe RestoreSummary,
    -- | Represents the version of
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GlobalTables.html global tables>
    -- in use, if the table is replicated across AWS Regions.
    globalTableVersion :: Prelude.Maybe Prelude.Text,
    -- | The total size of the specified table, in bytes. DynamoDB updates this
    -- value approximately every six hours. Recent changes might not be
    -- reflected in this value.
    tableSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | An array of @AttributeDefinition@ objects. Each of these objects
    -- describes one attribute in the table and index key schema.
    --
    -- Each @AttributeDefinition@ object in this array is composed of:
    --
    -- -   @AttributeName@ - The name of the attribute.
    --
    -- -   @AttributeType@ - The data type for the attribute.
    attributeDefinitions :: Prelude.Maybe [AttributeDefinition],
    -- | The Amazon Resource Name (ARN) that uniquely identifies the latest
    -- stream for this table.
    latestStreamArn :: Prelude.Maybe Prelude.Text,
    -- | The provisioned throughput settings for the table, consisting of read
    -- and write capacity units, along with data about increases and decreases.
    provisionedThroughput :: Prelude.Maybe ProvisionedThroughputDescription,
    -- | The current state of the table:
    --
    -- -   @CREATING@ - The table is being created.
    --
    -- -   @UPDATING@ - The table is being updated.
    --
    -- -   @DELETING@ - The table is being deleted.
    --
    -- -   @ACTIVE@ - The table is ready for use.
    --
    -- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The AWS KMS key used to
    --     encrypt the table in inaccessible. Table operations may fail due to
    --     failure to use the AWS KMS key. DynamoDB will initiate the table
    --     archival process when a table\'s AWS KMS key remains inaccessible
    --     for more than seven days.
    --
    -- -   @ARCHIVING@ - The table is being archived. Operations are not
    --     allowed until archival is complete.
    --
    -- -   @ARCHIVED@ - The table has been archived. See the ArchivalReason for
    --     more information.
    tableStatus :: Prelude.Maybe TableStatus,
    -- | The Amazon Resource Name (ARN) that uniquely identifies the table.
    tableArn :: Prelude.Maybe Prelude.Text,
    -- | The primary key structure for the table. Each @KeySchemaElement@
    -- consists of:
    --
    -- -   @AttributeName@ - The name of the attribute.
    --
    -- -   @KeyType@ - The role of the attribute:
    --
    --     -   @HASH@ - partition key
    --
    --     -   @RANGE@ - sort key
    --
    --     The partition key of an item is also known as its /hash attribute/.
    --     The term \"hash attribute\" derives from DynamoDB\'s usage of an
    --     internal hash function to evenly distribute data items across
    --     partitions, based on their partition key values.
    --
    --     The sort key of an item is also known as its /range attribute/. The
    --     term \"range attribute\" derives from the way DynamoDB stores items
    --     with the same partition key physically close together, in sorted
    --     order by the sort key value.
    --
    -- For more information about primary keys, see
    -- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html#DataModelPrimaryKey Primary Key>
    -- in the /Amazon DynamoDB Developer Guide/.
    keySchema :: Prelude.Maybe (Prelude.NonEmpty KeySchemaElement),
    -- | The global secondary indexes, if any, on the table. Each index is scoped
    -- to a given partition key value. Each element is composed of:
    --
    -- -   @Backfilling@ - If true, then the index is currently in the
    --     backfilling phase. Backfilling occurs only when a new global
    --     secondary index is added to the table. It is the process by which
    --     DynamoDB populates the new index with data from the table. (This
    --     attribute does not appear for indexes that were created during a
    --     @CreateTable@ operation.)
    --
    --     You can delete an index that is being created during the
    --     @Backfilling@ phase when @IndexStatus@ is set to CREATING and
    --     @Backfilling@ is true. You can\'t delete the index that is being
    --     created when @IndexStatus@ is set to CREATING and @Backfilling@ is
    --     false. (This attribute does not appear for indexes that were created
    --     during a @CreateTable@ operation.)
    --
    -- -   @IndexName@ - The name of the global secondary index.
    --
    -- -   @IndexSizeBytes@ - The total size of the global secondary index, in
    --     bytes. DynamoDB updates this value approximately every six hours.
    --     Recent changes might not be reflected in this value.
    --
    -- -   @IndexStatus@ - The current status of the global secondary index:
    --
    --     -   @CREATING@ - The index is being created.
    --
    --     -   @UPDATING@ - The index is being updated.
    --
    --     -   @DELETING@ - The index is being deleted.
    --
    --     -   @ACTIVE@ - The index is ready for use.
    --
    -- -   @ItemCount@ - The number of items in the global secondary index.
    --     DynamoDB updates this value approximately every six hours. Recent
    --     changes might not be reflected in this value.
    --
    -- -   @KeySchema@ - Specifies the complete index key schema. The attribute
    --     names in the key schema must be between 1 and 255 characters
    --     (inclusive). The key schema must begin with the same partition key
    --     as the table.
    --
    -- -   @Projection@ - Specifies attributes that are copied (projected) from
    --     the table into the index. These are in addition to the primary key
    --     attributes and index key attributes, which are automatically
    --     projected. Each attribute specification is composed of:
    --
    --     -   @ProjectionType@ - One of the following:
    --
    --         -   @KEYS_ONLY@ - Only the index and primary keys are projected
    --             into the index.
    --
    --         -   @INCLUDE@ - In addition to the attributes described in
    --             @KEYS_ONLY@, the secondary index will include other non-key
    --             attributes that you specify.
    --
    --         -   @ALL@ - All of the table attributes are projected into the
    --             index.
    --
    --     -   @NonKeyAttributes@ - A list of one or more non-key attribute
    --         names that are projected into the secondary index. The total
    --         count of attributes provided in @NonKeyAttributes@, summed
    --         across all of the secondary indexes, must not exceed 20. If you
    --         project the same attribute into two different indexes, this
    --         counts as two distinct attributes when determining the total.
    --
    -- -   @ProvisionedThroughput@ - The provisioned throughput settings for
    --     the global secondary index, consisting of read and write capacity
    --     units, along with data about increases and decreases.
    --
    -- If the table is in the @DELETING@ state, no information about indexes
    -- will be returned.
    globalSecondaryIndexes :: Prelude.Maybe [GlobalSecondaryIndexDescription],
    -- | A timestamp, in ISO 8601 format, for this stream.
    --
    -- Note that @LatestStreamLabel@ is not a unique identifier for the stream,
    -- because it is possible that a stream from another table might have the
    -- same timestamp. However, the combination of the following three elements
    -- is guaranteed to be unique:
    --
    -- -   AWS customer ID
    --
    -- -   Table name
    --
    -- -   @StreamLabel@
    latestStreamLabel :: Prelude.Maybe Prelude.Text,
    -- | Contains the details for the read\/write capacity mode.
    billingModeSummary :: Prelude.Maybe BillingModeSummary,
    -- | Represents one or more local secondary indexes on the table. Each index
    -- is scoped to a given partition key value. Tables with one or more local
    -- secondary indexes are subject to an item collection size limit, where
    -- the amount of data within a given item collection cannot exceed 10 GB.
    -- Each element is composed of:
    --
    -- -   @IndexName@ - The name of the local secondary index.
    --
    -- -   @KeySchema@ - Specifies the complete index key schema. The attribute
    --     names in the key schema must be between 1 and 255 characters
    --     (inclusive). The key schema must begin with the same partition key
    --     as the table.
    --
    -- -   @Projection@ - Specifies attributes that are copied (projected) from
    --     the table into the index. These are in addition to the primary key
    --     attributes and index key attributes, which are automatically
    --     projected. Each attribute specification is composed of:
    --
    --     -   @ProjectionType@ - One of the following:
    --
    --         -   @KEYS_ONLY@ - Only the index and primary keys are projected
    --             into the index.
    --
    --         -   @INCLUDE@ - Only the specified table attributes are
    --             projected into the index. The list of projected attributes
    --             is in @NonKeyAttributes@.
    --
    --         -   @ALL@ - All of the table attributes are projected into the
    --             index.
    --
    --     -   @NonKeyAttributes@ - A list of one or more non-key attribute
    --         names that are projected into the secondary index. The total
    --         count of attributes provided in @NonKeyAttributes@, summed
    --         across all of the secondary indexes, must not exceed 20. If you
    --         project the same attribute into two different indexes, this
    --         counts as two distinct attributes when determining the total.
    --
    -- -   @IndexSizeBytes@ - Represents the total size of the index, in bytes.
    --     DynamoDB updates this value approximately every six hours. Recent
    --     changes might not be reflected in this value.
    --
    -- -   @ItemCount@ - Represents the number of items in the index. DynamoDB
    --     updates this value approximately every six hours. Recent changes
    --     might not be reflected in this value.
    --
    -- If the table is in the @DELETING@ state, no information about indexes
    -- will be returned.
    localSecondaryIndexes :: Prelude.Maybe [LocalSecondaryIndexDescription],
    -- | The date and time when the table was created, in
    -- <http://www.epochconverter.com/ UNIX epoch time> format.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The description of the server-side encryption status on the specified
    -- table.
    sSEDescription :: Prelude.Maybe SSEDescription,
    -- | Unique identifier for the table for which the backup was created.
    tableId :: Prelude.Maybe Prelude.Text,
    -- | Represents replicas of the table.
    replicas :: Prelude.Maybe [ReplicaDescription],
    -- | The number of items in the specified table. DynamoDB updates this value
    -- approximately every six hours. Recent changes might not be reflected in
    -- this value.
    itemCount :: Prelude.Maybe Prelude.Integer,
    -- | Contains information about the table archive.
    archivalSummary :: Prelude.Maybe ArchivalSummary,
    -- | The name of the table.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The current DynamoDB Streams configuration for the table.
    streamSpecification :: Prelude.Maybe StreamSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restoreSummary', 'tableDescription_restoreSummary' - Contains details for the restore.
--
-- 'globalTableVersion', 'tableDescription_globalTableVersion' - Represents the version of
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GlobalTables.html global tables>
-- in use, if the table is replicated across AWS Regions.
--
-- 'tableSizeBytes', 'tableDescription_tableSizeBytes' - The total size of the specified table, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be
-- reflected in this value.
--
-- 'attributeDefinitions', 'tableDescription_attributeDefinitions' - An array of @AttributeDefinition@ objects. Each of these objects
-- describes one attribute in the table and index key schema.
--
-- Each @AttributeDefinition@ object in this array is composed of:
--
-- -   @AttributeName@ - The name of the attribute.
--
-- -   @AttributeType@ - The data type for the attribute.
--
-- 'latestStreamArn', 'tableDescription_latestStreamArn' - The Amazon Resource Name (ARN) that uniquely identifies the latest
-- stream for this table.
--
-- 'provisionedThroughput', 'tableDescription_provisionedThroughput' - The provisioned throughput settings for the table, consisting of read
-- and write capacity units, along with data about increases and decreases.
--
-- 'tableStatus', 'tableDescription_tableStatus' - The current state of the table:
--
-- -   @CREATING@ - The table is being created.
--
-- -   @UPDATING@ - The table is being updated.
--
-- -   @DELETING@ - The table is being deleted.
--
-- -   @ACTIVE@ - The table is ready for use.
--
-- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The AWS KMS key used to
--     encrypt the table in inaccessible. Table operations may fail due to
--     failure to use the AWS KMS key. DynamoDB will initiate the table
--     archival process when a table\'s AWS KMS key remains inaccessible
--     for more than seven days.
--
-- -   @ARCHIVING@ - The table is being archived. Operations are not
--     allowed until archival is complete.
--
-- -   @ARCHIVED@ - The table has been archived. See the ArchivalReason for
--     more information.
--
-- 'tableArn', 'tableDescription_tableArn' - The Amazon Resource Name (ARN) that uniquely identifies the table.
--
-- 'keySchema', 'tableDescription_keySchema' - The primary key structure for the table. Each @KeySchemaElement@
-- consists of:
--
-- -   @AttributeName@ - The name of the attribute.
--
-- -   @KeyType@ - The role of the attribute:
--
--     -   @HASH@ - partition key
--
--     -   @RANGE@ - sort key
--
--     The partition key of an item is also known as its /hash attribute/.
--     The term \"hash attribute\" derives from DynamoDB\'s usage of an
--     internal hash function to evenly distribute data items across
--     partitions, based on their partition key values.
--
--     The sort key of an item is also known as its /range attribute/. The
--     term \"range attribute\" derives from the way DynamoDB stores items
--     with the same partition key physically close together, in sorted
--     order by the sort key value.
--
-- For more information about primary keys, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html#DataModelPrimaryKey Primary Key>
-- in the /Amazon DynamoDB Developer Guide/.
--
-- 'globalSecondaryIndexes', 'tableDescription_globalSecondaryIndexes' - The global secondary indexes, if any, on the table. Each index is scoped
-- to a given partition key value. Each element is composed of:
--
-- -   @Backfilling@ - If true, then the index is currently in the
--     backfilling phase. Backfilling occurs only when a new global
--     secondary index is added to the table. It is the process by which
--     DynamoDB populates the new index with data from the table. (This
--     attribute does not appear for indexes that were created during a
--     @CreateTable@ operation.)
--
--     You can delete an index that is being created during the
--     @Backfilling@ phase when @IndexStatus@ is set to CREATING and
--     @Backfilling@ is true. You can\'t delete the index that is being
--     created when @IndexStatus@ is set to CREATING and @Backfilling@ is
--     false. (This attribute does not appear for indexes that were created
--     during a @CreateTable@ operation.)
--
-- -   @IndexName@ - The name of the global secondary index.
--
-- -   @IndexSizeBytes@ - The total size of the global secondary index, in
--     bytes. DynamoDB updates this value approximately every six hours.
--     Recent changes might not be reflected in this value.
--
-- -   @IndexStatus@ - The current status of the global secondary index:
--
--     -   @CREATING@ - The index is being created.
--
--     -   @UPDATING@ - The index is being updated.
--
--     -   @DELETING@ - The index is being deleted.
--
--     -   @ACTIVE@ - The index is ready for use.
--
-- -   @ItemCount@ - The number of items in the global secondary index.
--     DynamoDB updates this value approximately every six hours. Recent
--     changes might not be reflected in this value.
--
-- -   @KeySchema@ - Specifies the complete index key schema. The attribute
--     names in the key schema must be between 1 and 255 characters
--     (inclusive). The key schema must begin with the same partition key
--     as the table.
--
-- -   @Projection@ - Specifies attributes that are copied (projected) from
--     the table into the index. These are in addition to the primary key
--     attributes and index key attributes, which are automatically
--     projected. Each attribute specification is composed of:
--
--     -   @ProjectionType@ - One of the following:
--
--         -   @KEYS_ONLY@ - Only the index and primary keys are projected
--             into the index.
--
--         -   @INCLUDE@ - In addition to the attributes described in
--             @KEYS_ONLY@, the secondary index will include other non-key
--             attributes that you specify.
--
--         -   @ALL@ - All of the table attributes are projected into the
--             index.
--
--     -   @NonKeyAttributes@ - A list of one or more non-key attribute
--         names that are projected into the secondary index. The total
--         count of attributes provided in @NonKeyAttributes@, summed
--         across all of the secondary indexes, must not exceed 20. If you
--         project the same attribute into two different indexes, this
--         counts as two distinct attributes when determining the total.
--
-- -   @ProvisionedThroughput@ - The provisioned throughput settings for
--     the global secondary index, consisting of read and write capacity
--     units, along with data about increases and decreases.
--
-- If the table is in the @DELETING@ state, no information about indexes
-- will be returned.
--
-- 'latestStreamLabel', 'tableDescription_latestStreamLabel' - A timestamp, in ISO 8601 format, for this stream.
--
-- Note that @LatestStreamLabel@ is not a unique identifier for the stream,
-- because it is possible that a stream from another table might have the
-- same timestamp. However, the combination of the following three elements
-- is guaranteed to be unique:
--
-- -   AWS customer ID
--
-- -   Table name
--
-- -   @StreamLabel@
--
-- 'billingModeSummary', 'tableDescription_billingModeSummary' - Contains the details for the read\/write capacity mode.
--
-- 'localSecondaryIndexes', 'tableDescription_localSecondaryIndexes' - Represents one or more local secondary indexes on the table. Each index
-- is scoped to a given partition key value. Tables with one or more local
-- secondary indexes are subject to an item collection size limit, where
-- the amount of data within a given item collection cannot exceed 10 GB.
-- Each element is composed of:
--
-- -   @IndexName@ - The name of the local secondary index.
--
-- -   @KeySchema@ - Specifies the complete index key schema. The attribute
--     names in the key schema must be between 1 and 255 characters
--     (inclusive). The key schema must begin with the same partition key
--     as the table.
--
-- -   @Projection@ - Specifies attributes that are copied (projected) from
--     the table into the index. These are in addition to the primary key
--     attributes and index key attributes, which are automatically
--     projected. Each attribute specification is composed of:
--
--     -   @ProjectionType@ - One of the following:
--
--         -   @KEYS_ONLY@ - Only the index and primary keys are projected
--             into the index.
--
--         -   @INCLUDE@ - Only the specified table attributes are
--             projected into the index. The list of projected attributes
--             is in @NonKeyAttributes@.
--
--         -   @ALL@ - All of the table attributes are projected into the
--             index.
--
--     -   @NonKeyAttributes@ - A list of one or more non-key attribute
--         names that are projected into the secondary index. The total
--         count of attributes provided in @NonKeyAttributes@, summed
--         across all of the secondary indexes, must not exceed 20. If you
--         project the same attribute into two different indexes, this
--         counts as two distinct attributes when determining the total.
--
-- -   @IndexSizeBytes@ - Represents the total size of the index, in bytes.
--     DynamoDB updates this value approximately every six hours. Recent
--     changes might not be reflected in this value.
--
-- -   @ItemCount@ - Represents the number of items in the index. DynamoDB
--     updates this value approximately every six hours. Recent changes
--     might not be reflected in this value.
--
-- If the table is in the @DELETING@ state, no information about indexes
-- will be returned.
--
-- 'creationDateTime', 'tableDescription_creationDateTime' - The date and time when the table was created, in
-- <http://www.epochconverter.com/ UNIX epoch time> format.
--
-- 'sSEDescription', 'tableDescription_sSEDescription' - The description of the server-side encryption status on the specified
-- table.
--
-- 'tableId', 'tableDescription_tableId' - Unique identifier for the table for which the backup was created.
--
-- 'replicas', 'tableDescription_replicas' - Represents replicas of the table.
--
-- 'itemCount', 'tableDescription_itemCount' - The number of items in the specified table. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
--
-- 'archivalSummary', 'tableDescription_archivalSummary' - Contains information about the table archive.
--
-- 'tableName', 'tableDescription_tableName' - The name of the table.
--
-- 'streamSpecification', 'tableDescription_streamSpecification' - The current DynamoDB Streams configuration for the table.
newTableDescription ::
  TableDescription
newTableDescription =
  TableDescription'
    { restoreSummary = Prelude.Nothing,
      globalTableVersion = Prelude.Nothing,
      tableSizeBytes = Prelude.Nothing,
      attributeDefinitions = Prelude.Nothing,
      latestStreamArn = Prelude.Nothing,
      provisionedThroughput = Prelude.Nothing,
      tableStatus = Prelude.Nothing,
      tableArn = Prelude.Nothing,
      keySchema = Prelude.Nothing,
      globalSecondaryIndexes = Prelude.Nothing,
      latestStreamLabel = Prelude.Nothing,
      billingModeSummary = Prelude.Nothing,
      localSecondaryIndexes = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      sSEDescription = Prelude.Nothing,
      tableId = Prelude.Nothing,
      replicas = Prelude.Nothing,
      itemCount = Prelude.Nothing,
      archivalSummary = Prelude.Nothing,
      tableName = Prelude.Nothing,
      streamSpecification = Prelude.Nothing
    }

-- | Contains details for the restore.
tableDescription_restoreSummary :: Lens.Lens' TableDescription (Prelude.Maybe RestoreSummary)
tableDescription_restoreSummary = Lens.lens (\TableDescription' {restoreSummary} -> restoreSummary) (\s@TableDescription' {} a -> s {restoreSummary = a} :: TableDescription)

-- | Represents the version of
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/GlobalTables.html global tables>
-- in use, if the table is replicated across AWS Regions.
tableDescription_globalTableVersion :: Lens.Lens' TableDescription (Prelude.Maybe Prelude.Text)
tableDescription_globalTableVersion = Lens.lens (\TableDescription' {globalTableVersion} -> globalTableVersion) (\s@TableDescription' {} a -> s {globalTableVersion = a} :: TableDescription)

-- | The total size of the specified table, in bytes. DynamoDB updates this
-- value approximately every six hours. Recent changes might not be
-- reflected in this value.
tableDescription_tableSizeBytes :: Lens.Lens' TableDescription (Prelude.Maybe Prelude.Integer)
tableDescription_tableSizeBytes = Lens.lens (\TableDescription' {tableSizeBytes} -> tableSizeBytes) (\s@TableDescription' {} a -> s {tableSizeBytes = a} :: TableDescription)

-- | An array of @AttributeDefinition@ objects. Each of these objects
-- describes one attribute in the table and index key schema.
--
-- Each @AttributeDefinition@ object in this array is composed of:
--
-- -   @AttributeName@ - The name of the attribute.
--
-- -   @AttributeType@ - The data type for the attribute.
tableDescription_attributeDefinitions :: Lens.Lens' TableDescription (Prelude.Maybe [AttributeDefinition])
tableDescription_attributeDefinitions = Lens.lens (\TableDescription' {attributeDefinitions} -> attributeDefinitions) (\s@TableDescription' {} a -> s {attributeDefinitions = a} :: TableDescription) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) that uniquely identifies the latest
-- stream for this table.
tableDescription_latestStreamArn :: Lens.Lens' TableDescription (Prelude.Maybe Prelude.Text)
tableDescription_latestStreamArn = Lens.lens (\TableDescription' {latestStreamArn} -> latestStreamArn) (\s@TableDescription' {} a -> s {latestStreamArn = a} :: TableDescription)

-- | The provisioned throughput settings for the table, consisting of read
-- and write capacity units, along with data about increases and decreases.
tableDescription_provisionedThroughput :: Lens.Lens' TableDescription (Prelude.Maybe ProvisionedThroughputDescription)
tableDescription_provisionedThroughput = Lens.lens (\TableDescription' {provisionedThroughput} -> provisionedThroughput) (\s@TableDescription' {} a -> s {provisionedThroughput = a} :: TableDescription)

-- | The current state of the table:
--
-- -   @CREATING@ - The table is being created.
--
-- -   @UPDATING@ - The table is being updated.
--
-- -   @DELETING@ - The table is being deleted.
--
-- -   @ACTIVE@ - The table is ready for use.
--
-- -   @INACCESSIBLE_ENCRYPTION_CREDENTIALS@ - The AWS KMS key used to
--     encrypt the table in inaccessible. Table operations may fail due to
--     failure to use the AWS KMS key. DynamoDB will initiate the table
--     archival process when a table\'s AWS KMS key remains inaccessible
--     for more than seven days.
--
-- -   @ARCHIVING@ - The table is being archived. Operations are not
--     allowed until archival is complete.
--
-- -   @ARCHIVED@ - The table has been archived. See the ArchivalReason for
--     more information.
tableDescription_tableStatus :: Lens.Lens' TableDescription (Prelude.Maybe TableStatus)
tableDescription_tableStatus = Lens.lens (\TableDescription' {tableStatus} -> tableStatus) (\s@TableDescription' {} a -> s {tableStatus = a} :: TableDescription)

-- | The Amazon Resource Name (ARN) that uniquely identifies the table.
tableDescription_tableArn :: Lens.Lens' TableDescription (Prelude.Maybe Prelude.Text)
tableDescription_tableArn = Lens.lens (\TableDescription' {tableArn} -> tableArn) (\s@TableDescription' {} a -> s {tableArn = a} :: TableDescription)

-- | The primary key structure for the table. Each @KeySchemaElement@
-- consists of:
--
-- -   @AttributeName@ - The name of the attribute.
--
-- -   @KeyType@ - The role of the attribute:
--
--     -   @HASH@ - partition key
--
--     -   @RANGE@ - sort key
--
--     The partition key of an item is also known as its /hash attribute/.
--     The term \"hash attribute\" derives from DynamoDB\'s usage of an
--     internal hash function to evenly distribute data items across
--     partitions, based on their partition key values.
--
--     The sort key of an item is also known as its /range attribute/. The
--     term \"range attribute\" derives from the way DynamoDB stores items
--     with the same partition key physically close together, in sorted
--     order by the sort key value.
--
-- For more information about primary keys, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/DataModel.html#DataModelPrimaryKey Primary Key>
-- in the /Amazon DynamoDB Developer Guide/.
tableDescription_keySchema :: Lens.Lens' TableDescription (Prelude.Maybe (Prelude.NonEmpty KeySchemaElement))
tableDescription_keySchema = Lens.lens (\TableDescription' {keySchema} -> keySchema) (\s@TableDescription' {} a -> s {keySchema = a} :: TableDescription) Prelude.. Lens.mapping Lens.coerced

-- | The global secondary indexes, if any, on the table. Each index is scoped
-- to a given partition key value. Each element is composed of:
--
-- -   @Backfilling@ - If true, then the index is currently in the
--     backfilling phase. Backfilling occurs only when a new global
--     secondary index is added to the table. It is the process by which
--     DynamoDB populates the new index with data from the table. (This
--     attribute does not appear for indexes that were created during a
--     @CreateTable@ operation.)
--
--     You can delete an index that is being created during the
--     @Backfilling@ phase when @IndexStatus@ is set to CREATING and
--     @Backfilling@ is true. You can\'t delete the index that is being
--     created when @IndexStatus@ is set to CREATING and @Backfilling@ is
--     false. (This attribute does not appear for indexes that were created
--     during a @CreateTable@ operation.)
--
-- -   @IndexName@ - The name of the global secondary index.
--
-- -   @IndexSizeBytes@ - The total size of the global secondary index, in
--     bytes. DynamoDB updates this value approximately every six hours.
--     Recent changes might not be reflected in this value.
--
-- -   @IndexStatus@ - The current status of the global secondary index:
--
--     -   @CREATING@ - The index is being created.
--
--     -   @UPDATING@ - The index is being updated.
--
--     -   @DELETING@ - The index is being deleted.
--
--     -   @ACTIVE@ - The index is ready for use.
--
-- -   @ItemCount@ - The number of items in the global secondary index.
--     DynamoDB updates this value approximately every six hours. Recent
--     changes might not be reflected in this value.
--
-- -   @KeySchema@ - Specifies the complete index key schema. The attribute
--     names in the key schema must be between 1 and 255 characters
--     (inclusive). The key schema must begin with the same partition key
--     as the table.
--
-- -   @Projection@ - Specifies attributes that are copied (projected) from
--     the table into the index. These are in addition to the primary key
--     attributes and index key attributes, which are automatically
--     projected. Each attribute specification is composed of:
--
--     -   @ProjectionType@ - One of the following:
--
--         -   @KEYS_ONLY@ - Only the index and primary keys are projected
--             into the index.
--
--         -   @INCLUDE@ - In addition to the attributes described in
--             @KEYS_ONLY@, the secondary index will include other non-key
--             attributes that you specify.
--
--         -   @ALL@ - All of the table attributes are projected into the
--             index.
--
--     -   @NonKeyAttributes@ - A list of one or more non-key attribute
--         names that are projected into the secondary index. The total
--         count of attributes provided in @NonKeyAttributes@, summed
--         across all of the secondary indexes, must not exceed 20. If you
--         project the same attribute into two different indexes, this
--         counts as two distinct attributes when determining the total.
--
-- -   @ProvisionedThroughput@ - The provisioned throughput settings for
--     the global secondary index, consisting of read and write capacity
--     units, along with data about increases and decreases.
--
-- If the table is in the @DELETING@ state, no information about indexes
-- will be returned.
tableDescription_globalSecondaryIndexes :: Lens.Lens' TableDescription (Prelude.Maybe [GlobalSecondaryIndexDescription])
tableDescription_globalSecondaryIndexes = Lens.lens (\TableDescription' {globalSecondaryIndexes} -> globalSecondaryIndexes) (\s@TableDescription' {} a -> s {globalSecondaryIndexes = a} :: TableDescription) Prelude.. Lens.mapping Lens.coerced

-- | A timestamp, in ISO 8601 format, for this stream.
--
-- Note that @LatestStreamLabel@ is not a unique identifier for the stream,
-- because it is possible that a stream from another table might have the
-- same timestamp. However, the combination of the following three elements
-- is guaranteed to be unique:
--
-- -   AWS customer ID
--
-- -   Table name
--
-- -   @StreamLabel@
tableDescription_latestStreamLabel :: Lens.Lens' TableDescription (Prelude.Maybe Prelude.Text)
tableDescription_latestStreamLabel = Lens.lens (\TableDescription' {latestStreamLabel} -> latestStreamLabel) (\s@TableDescription' {} a -> s {latestStreamLabel = a} :: TableDescription)

-- | Contains the details for the read\/write capacity mode.
tableDescription_billingModeSummary :: Lens.Lens' TableDescription (Prelude.Maybe BillingModeSummary)
tableDescription_billingModeSummary = Lens.lens (\TableDescription' {billingModeSummary} -> billingModeSummary) (\s@TableDescription' {} a -> s {billingModeSummary = a} :: TableDescription)

-- | Represents one or more local secondary indexes on the table. Each index
-- is scoped to a given partition key value. Tables with one or more local
-- secondary indexes are subject to an item collection size limit, where
-- the amount of data within a given item collection cannot exceed 10 GB.
-- Each element is composed of:
--
-- -   @IndexName@ - The name of the local secondary index.
--
-- -   @KeySchema@ - Specifies the complete index key schema. The attribute
--     names in the key schema must be between 1 and 255 characters
--     (inclusive). The key schema must begin with the same partition key
--     as the table.
--
-- -   @Projection@ - Specifies attributes that are copied (projected) from
--     the table into the index. These are in addition to the primary key
--     attributes and index key attributes, which are automatically
--     projected. Each attribute specification is composed of:
--
--     -   @ProjectionType@ - One of the following:
--
--         -   @KEYS_ONLY@ - Only the index and primary keys are projected
--             into the index.
--
--         -   @INCLUDE@ - Only the specified table attributes are
--             projected into the index. The list of projected attributes
--             is in @NonKeyAttributes@.
--
--         -   @ALL@ - All of the table attributes are projected into the
--             index.
--
--     -   @NonKeyAttributes@ - A list of one or more non-key attribute
--         names that are projected into the secondary index. The total
--         count of attributes provided in @NonKeyAttributes@, summed
--         across all of the secondary indexes, must not exceed 20. If you
--         project the same attribute into two different indexes, this
--         counts as two distinct attributes when determining the total.
--
-- -   @IndexSizeBytes@ - Represents the total size of the index, in bytes.
--     DynamoDB updates this value approximately every six hours. Recent
--     changes might not be reflected in this value.
--
-- -   @ItemCount@ - Represents the number of items in the index. DynamoDB
--     updates this value approximately every six hours. Recent changes
--     might not be reflected in this value.
--
-- If the table is in the @DELETING@ state, no information about indexes
-- will be returned.
tableDescription_localSecondaryIndexes :: Lens.Lens' TableDescription (Prelude.Maybe [LocalSecondaryIndexDescription])
tableDescription_localSecondaryIndexes = Lens.lens (\TableDescription' {localSecondaryIndexes} -> localSecondaryIndexes) (\s@TableDescription' {} a -> s {localSecondaryIndexes = a} :: TableDescription) Prelude.. Lens.mapping Lens.coerced

-- | The date and time when the table was created, in
-- <http://www.epochconverter.com/ UNIX epoch time> format.
tableDescription_creationDateTime :: Lens.Lens' TableDescription (Prelude.Maybe Prelude.UTCTime)
tableDescription_creationDateTime = Lens.lens (\TableDescription' {creationDateTime} -> creationDateTime) (\s@TableDescription' {} a -> s {creationDateTime = a} :: TableDescription) Prelude.. Lens.mapping Core._Time

-- | The description of the server-side encryption status on the specified
-- table.
tableDescription_sSEDescription :: Lens.Lens' TableDescription (Prelude.Maybe SSEDescription)
tableDescription_sSEDescription = Lens.lens (\TableDescription' {sSEDescription} -> sSEDescription) (\s@TableDescription' {} a -> s {sSEDescription = a} :: TableDescription)

-- | Unique identifier for the table for which the backup was created.
tableDescription_tableId :: Lens.Lens' TableDescription (Prelude.Maybe Prelude.Text)
tableDescription_tableId = Lens.lens (\TableDescription' {tableId} -> tableId) (\s@TableDescription' {} a -> s {tableId = a} :: TableDescription)

-- | Represents replicas of the table.
tableDescription_replicas :: Lens.Lens' TableDescription (Prelude.Maybe [ReplicaDescription])
tableDescription_replicas = Lens.lens (\TableDescription' {replicas} -> replicas) (\s@TableDescription' {} a -> s {replicas = a} :: TableDescription) Prelude.. Lens.mapping Lens.coerced

-- | The number of items in the specified table. DynamoDB updates this value
-- approximately every six hours. Recent changes might not be reflected in
-- this value.
tableDescription_itemCount :: Lens.Lens' TableDescription (Prelude.Maybe Prelude.Integer)
tableDescription_itemCount = Lens.lens (\TableDescription' {itemCount} -> itemCount) (\s@TableDescription' {} a -> s {itemCount = a} :: TableDescription)

-- | Contains information about the table archive.
tableDescription_archivalSummary :: Lens.Lens' TableDescription (Prelude.Maybe ArchivalSummary)
tableDescription_archivalSummary = Lens.lens (\TableDescription' {archivalSummary} -> archivalSummary) (\s@TableDescription' {} a -> s {archivalSummary = a} :: TableDescription)

-- | The name of the table.
tableDescription_tableName :: Lens.Lens' TableDescription (Prelude.Maybe Prelude.Text)
tableDescription_tableName = Lens.lens (\TableDescription' {tableName} -> tableName) (\s@TableDescription' {} a -> s {tableName = a} :: TableDescription)

-- | The current DynamoDB Streams configuration for the table.
tableDescription_streamSpecification :: Lens.Lens' TableDescription (Prelude.Maybe StreamSpecification)
tableDescription_streamSpecification = Lens.lens (\TableDescription' {streamSpecification} -> streamSpecification) (\s@TableDescription' {} a -> s {streamSpecification = a} :: TableDescription)

instance Core.FromJSON TableDescription where
  parseJSON =
    Core.withObject
      "TableDescription"
      ( \x ->
          TableDescription'
            Prelude.<$> (x Core..:? "RestoreSummary")
            Prelude.<*> (x Core..:? "GlobalTableVersion")
            Prelude.<*> (x Core..:? "TableSizeBytes")
            Prelude.<*> ( x Core..:? "AttributeDefinitions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "LatestStreamArn")
            Prelude.<*> (x Core..:? "ProvisionedThroughput")
            Prelude.<*> (x Core..:? "TableStatus")
            Prelude.<*> (x Core..:? "TableArn")
            Prelude.<*> (x Core..:? "KeySchema")
            Prelude.<*> ( x Core..:? "GlobalSecondaryIndexes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "LatestStreamLabel")
            Prelude.<*> (x Core..:? "BillingModeSummary")
            Prelude.<*> ( x Core..:? "LocalSecondaryIndexes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CreationDateTime")
            Prelude.<*> (x Core..:? "SSEDescription")
            Prelude.<*> (x Core..:? "TableId")
            Prelude.<*> (x Core..:? "Replicas" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ItemCount")
            Prelude.<*> (x Core..:? "ArchivalSummary")
            Prelude.<*> (x Core..:? "TableName")
            Prelude.<*> (x Core..:? "StreamSpecification")
      )

instance Prelude.Hashable TableDescription where
  hashWithSalt salt' TableDescription' {..} =
    salt' `Prelude.hashWithSalt` streamSpecification
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` archivalSummary
      `Prelude.hashWithSalt` itemCount
      `Prelude.hashWithSalt` replicas
      `Prelude.hashWithSalt` tableId
      `Prelude.hashWithSalt` sSEDescription
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` localSecondaryIndexes
      `Prelude.hashWithSalt` billingModeSummary
      `Prelude.hashWithSalt` latestStreamLabel
      `Prelude.hashWithSalt` globalSecondaryIndexes
      `Prelude.hashWithSalt` keySchema
      `Prelude.hashWithSalt` tableArn
      `Prelude.hashWithSalt` tableStatus
      `Prelude.hashWithSalt` provisionedThroughput
      `Prelude.hashWithSalt` latestStreamArn
      `Prelude.hashWithSalt` attributeDefinitions
      `Prelude.hashWithSalt` tableSizeBytes
      `Prelude.hashWithSalt` globalTableVersion
      `Prelude.hashWithSalt` restoreSummary

instance Prelude.NFData TableDescription where
  rnf TableDescription' {..} =
    Prelude.rnf restoreSummary
      `Prelude.seq` Prelude.rnf streamSpecification
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf archivalSummary
      `Prelude.seq` Prelude.rnf itemCount
      `Prelude.seq` Prelude.rnf replicas
      `Prelude.seq` Prelude.rnf tableId
      `Prelude.seq` Prelude.rnf sSEDescription
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf localSecondaryIndexes
      `Prelude.seq` Prelude.rnf billingModeSummary
      `Prelude.seq` Prelude.rnf latestStreamLabel
      `Prelude.seq` Prelude.rnf globalSecondaryIndexes
      `Prelude.seq` Prelude.rnf keySchema
      `Prelude.seq` Prelude.rnf tableArn
      `Prelude.seq` Prelude.rnf tableStatus
      `Prelude.seq` Prelude.rnf provisionedThroughput
      `Prelude.seq` Prelude.rnf latestStreamArn
      `Prelude.seq` Prelude.rnf attributeDefinitions
      `Prelude.seq` Prelude.rnf tableSizeBytes
      `Prelude.seq` Prelude.rnf globalTableVersion
